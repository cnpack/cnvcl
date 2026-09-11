{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2026 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnMarkDownView;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：Markdown 的虚拟化 VCL 显示控件实现单元
* 单元作者：CnPack 开发组
* 备    注：只维护视口附近的 RichEdit 宿主，超大 Block 按页加载 Unicode RTF，
*           并在流式更新时保持滚动锚点。滚动时循环复用仍可见的宿主，新宿主完成载入、
*           测高和定位后才显示，减少 RTF 重载和窗口闪动。
*           多个虚拟 RichEdit 宿主共享单一选择状态，切换宿主时清除旧选择。
*           焦点宿主移出虚拟视口前将焦点交还主视图，保证滚轮和双指滚动连续。
*           宿主换页时重置测量几何，并校验 RichEdit 回报高度，防止复用状态产生大块空白。
*           通用 TCnPadding/ Padding 属性提供四边内容留白，并同步参与折行测量、虚拟高度和滚动范围计算。
*           测量阶段使用隐藏的大客户区，最终定位后恢复实际格式矩形，保证代码块末行完整显示。
*           宿主复用时同时校验消息、块位置、BlockID、文档修订号和块修订号，避免局部消息重排后显示旧内容或沿用旧高度
*           局部消息更新只替换自身的虚拟项范围，保留下游条目已经测得的准确高度。
*           每个虚拟项保存独立背景色，默认按消息交替着色，也可通过事件提供自定义颜色。
*           通过 SelectedMessageIndex/SelectedItemIndex 暴露当前选中消息和条目，
*           支持消息追加或选中块原位追加后触发局部重排。
* 开发平台：PWin7 + Delphi 5
* 兼容测试：PWin7 + Delphi 2009 ~
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2026.09.09 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, Classes, SysUtils, Contnrs, Controls, Graphics, Forms,
  StdCtrls, ExtCtrls, ComCtrls, RichEdit, CnContainers, CnMarkDown;

const
  CN_MARKDOWN_DEFAULT_ALTERNATE_ITEM_COLOR = $00F8F8F8;

type
  TCnMarkDownView = class;

{$IFDEF SUPPORT_MARGIN_PADDING}
  { 新版 VCL 直接复用原生类型，保持通用类型名称。 }
  TCnMargins = TMargins;
  TCnPadding = TPadding;
{$ELSE}
  { 旧版 Delphi 提供可持久化的通用四边边距对象。 }
  TCnMargins = class(TPersistent)
  private
    FLeft: Integer;
    FTop: Integer;
    FRight: Integer;
    FBottom: Integer;
    FOnChange: TNotifyEvent;
    procedure SetLeft(Value: Integer);
    procedure SetTop(Value: Integer);
    procedure SetRight(Value: Integer);
    procedure SetBottom(Value: Integer);
  protected
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure SetBounds(ALeft, ATop, ARight, ABottom: Integer);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Left: Integer read FLeft write SetLeft default 8;
    property Top: Integer read FTop write SetTop default 8;
    property Right: Integer read FRight write SetRight default 8;
    property Bottom: Integer read FBottom write SetBottom default 8;
  end;

  TCnPadding = class(TCnMargins)
  end;
{$ENDIF}

  TCnMarkDownViewItem = class
  private
    FBackgroundColor: TColor;
  public
    MessageIndex: Integer;
    BlockIndex: Integer;
    BlockID: Int64;
    Revision: Cardinal;
    DocumentRevision: Cardinal;
    TextStart: Integer;
    TextLength: Integer;
    Height: Integer;
    property BackgroundColor: TColor read FBackgroundColor
      write FBackgroundColor;
  end;

  TCnMarkDownGetItemBackgroundColorEvent = procedure(Sender: TObject;
    Item: TCnMarkDownViewItem; var BackgroundColor: TColor) of object;

  TCnMarkDownRichHost = class(TRichEdit)
  private
    FView: TCnMarkDownView;
    FItemIndex: Integer;
    FMessageIndex: Integer;
    FBlockIndex: Integer;
    FBlockID: Int64;
    FBlockRevision: Cardinal;
    FDocumentRevision: Cardinal;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
  public
    property View: TCnMarkDownView read FView write FView;
    property ItemIndex: Integer read FItemIndex write FItemIndex;
    property MessageIndex: Integer read FMessageIndex write FMessageIndex;
    property BlockIndex: Integer read FBlockIndex write FBlockIndex;
    property BlockID: Int64 read FBlockID write FBlockID;
    property BlockRevision: Cardinal read FBlockRevision write FBlockRevision;
    property DocumentRevision: Cardinal read FDocumentRevision write FDocumentRevision;
  end;

  TCnMarkDownView = class(TCustomControl)
  private
    FFeed: TCnMarkDownFeed;
    FIndex: TCnVirtualHeightIndex;
    FItems: TObjectList;
    FMessageStarts: TCnIntegerList;
    FHosts: TList;
    FSelectedHost: TCnMarkDownRichHost;
    FMeasureHost: TCnMarkDownRichHost;
    FRequestedHeight: Integer;
    FFlushTimer: TTimer;
    FPendingMessage: Integer;
    FQueuedMessage: Integer;
    FPendingFinishMessage: Integer;
    FUpdateCount: Integer;
    FInLayout: Boolean;
    FHeightChanged: Boolean;
    FScrollOffset: Int64;
    FPageChars: Integer;
    FDefaultHeight: Integer;
    FItemSpacing: Integer;
    FHostBufferItems: Integer;
    FBackgroundColor: TColor;
    FAlternateItemColor: TColor;
    FOnGetItemBackgroundColor: TCnMarkDownGetItemBackgroundColorEvent;
{$IFNDEF SUPPORT_MARGIN_PADDING}
    FPadding: TCnPadding;
{$ENDIF}
    FLastPaddingLeft: Integer;
    FLastPaddingTop: Integer;
    FLastPaddingRight: Integer;
    FLastPaddingBottom: Integer;
    procedure SetFeed(Value: TCnMarkDownFeed);
    procedure SetPageChars(Value: Integer);
    procedure SetDefaultHeight(Value: Integer);
    procedure SetItemSpacing(Value: Integer);
    procedure SetScrollOffset(Value: Int64);
    procedure SetBackgroundColor(Value: TColor);
    procedure SetAlternateItemColor(Value: TColor);
    procedure SetOnGetItemBackgroundColor(
      Value: TCnMarkDownGetItemBackgroundColorEvent);
{$IFNDEF SUPPORT_MARGIN_PADDING}
    procedure SetPadding(Value: TCnPadding);
    procedure PaddingChanged(Sender: TObject);
{$ENDIF}
    procedure CheckPaddingChanged;
    function GetPaddingLeft: Integer;
    function GetPaddingTop: Integer;
    function GetPaddingRight: Integer;
    function GetPaddingBottom: Integer;
    function GetContentWidth: Integer;
    function ResolveItemBackgroundColor(Item: TCnMarkDownViewItem): TColor;
    procedure FeedChanged(Sender: TObject; ChangeType: TCnMarkDownFeedChangeType;
      MessageIndex: Integer);
    procedure FlushTimer(Sender: TObject);
    procedure ClearHosts;
    procedure EnsureHosts;
    procedure ReorderHosts(StartIndex: Integer);
    procedure RebuildAll;
    procedure RebuildFromMessage(MessageIndex: Integer);
    procedure RebuildIndex;
    procedure AddMessageItems(MessageIndex: Integer);
    function AddBlockItems(MessageIndex, BlockIndex: Integer): Integer;
    function EstimateHeight(Block: TCnMarkDownBlock; StartPos, TextLength: Integer): Integer;
    function FindPageLength(const Text: TCnMarkDownText; StartPos: Integer): Integer;
    function FindItem(MessageIndex, BlockIndex, TextStart: Integer): Integer;
    function MaxScroll: Int64;
    function GetTotalHeight: Int64;
    function GetItemCount: Integer;
    function ScrollToPosition(Value: Int64): Int64;
    function ScrollFromPosition(Value: Integer): Int64;
    procedure UpdateScrollBar;
    function LineHeight: Integer;
    procedure LayoutHosts;
    procedure ReleaseHostFocus(Host: TCnMarkDownRichHost);
    procedure ActivateHost(Host: TCnMarkDownRichHost);
    function GetSelectedItemIndex: Integer;
    function GetSelectedMessageIndex: Integer;
    procedure SetHostFormatRect(Host: TCnMarkDownRichHost);
    procedure PrepareHostMeasure(Host: TCnMarkDownRichHost);
    function ValidateHostHeight(Host: TCnMarkDownRichHost;
      Item: TCnMarkDownViewItem; Value: Integer): Integer;
    procedure BindHost(Host: TCnMarkDownRichHost; ItemIndex: Integer);
    procedure LoadHostBlock(Host: TCnMarkDownRichHost; Item: TCnMarkDownViewItem);
    procedure UpdateItemHeight(ItemIndex, Value: Integer);
    procedure SetPendingMessage(MessageIndex: Integer);
    procedure ScrollBy(Delta: Int64);
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
{$IFDEF SUPPORT_MARGIN_PADDING}
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
{$ENDIF}
    procedure Paint; override;
    procedure Resize; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Flush;
    function AddMessage(ARole: TCnMarkDownMessageRole): Integer;
    procedure AppendToMessage(MessageIndex: Integer;
      const AChunk: TCnMarkDownText);
    procedure FinishMessage(MessageIndex: Integer);
    procedure AppendToItem(ItemIndex: Integer;
      const AChunk: TCnMarkDownText);
    procedure AppendToSelectedItem(const AChunk: TCnMarkDownText);
    procedure ScrollToBottom;
    procedure RefreshItemBackgroundColors;
    property Feed: TCnMarkDownFeed read FFeed write SetFeed;
    property TotalHeight: Int64 read GetTotalHeight;
    property ItemCount: Integer read GetItemCount;
    property SelectedItemIndex: Integer read GetSelectedItemIndex;
    property SelectedMessageIndex: Integer read GetSelectedMessageIndex;
  published
    property Align;
    property Anchors;
    property Color: TColor read FBackgroundColor write SetBackgroundColor default clWindow;
    property AlternateItemColor: TColor read FAlternateItemColor
      write SetAlternateItemColor default CN_MARKDOWN_DEFAULT_ALTERNATE_ITEM_COLOR;
    property Font;
    property TabStop;
    property Visible;
    property Enabled;
{$IFDEF SUPPORT_MARGIN_PADDING}
    property Padding;
{$ELSE}
    property Padding: TCnPadding read FPadding write SetPadding;
{$ENDIF}
    property PageChars: Integer read FPageChars write SetPageChars default 65536;
    property DefaultItemHeight: Integer read FDefaultHeight write SetDefaultHeight default 24;
    property ItemSpacing: Integer read FItemSpacing write SetItemSpacing default 4;
    property ScrollOffset: Int64 read FScrollOffset write SetScrollOffset;
    property OnGetItemBackgroundColor: TCnMarkDownGetItemBackgroundColorEvent
      read FOnGetItemBackgroundColor write SetOnGetItemBackgroundColor;
  end;

implementation

{$IFNDEF SUPPORT_MARGIN_PADDING}

constructor TCnMargins.Create;
begin
  inherited Create;
  FLeft := 8;
  FTop := 8;
  FRight := 8;
  FBottom := 8;
end;

procedure TCnMargins.Assign(Source: TPersistent);
var
  SourceMargins: TCnMargins;
begin
  if Source is TCnMargins then
  begin
    SourceMargins := TCnMargins(Source);
    FLeft := SourceMargins.Left;
    FTop := SourceMargins.Top;
    FRight := SourceMargins.Right;
    FBottom := SourceMargins.Bottom;
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure TCnMargins.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCnMargins.SetLeft(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if FLeft <> Value then
  begin
    FLeft := Value;
    Changed;
  end;
end;

procedure TCnMargins.SetTop(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if FTop <> Value then
  begin
    FTop := Value;
    Changed;
  end;
end;

procedure TCnMargins.SetRight(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if FRight <> Value then
  begin
    FRight := Value;
    Changed;
  end;
end;

procedure TCnMargins.SetBottom(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if FBottom <> Value then
  begin
    FBottom := Value;
    Changed;
  end;
end;

procedure TCnMargins.SetBounds(ALeft, ATop, ARight, ABottom: Integer);
begin
  if ALeft < 0 then
    ALeft := 0;
  if ATop < 0 then
    ATop := 0;
  if ARight < 0 then
    ARight := 0;
  if ABottom < 0 then
    ABottom := 0;
  if (FLeft = ALeft) and (FTop = ATop) and
    (FRight = ARight) and (FBottom = ABottom) then
    Exit;
  FLeft := ALeft;
  FTop := ATop;
  FRight := ARight;
  FBottom := ABottom;
  Changed;
end;

{$ENDIF}

constructor TCnMarkDownView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 400;
  Height := 300;
  ControlStyle := ControlStyle + [csOpaque, csDoubleClicks];
  DoubleBuffered := True;
  TabStop := True;
  FBackgroundColor := clWindow;
  FAlternateItemColor := CN_MARKDOWN_DEFAULT_ALTERNATE_ITEM_COLOR;
  FFeed := nil;
  FIndex := TCnVirtualHeightIndex.Create;
  FItems := TObjectList.Create(True);
  FMessageStarts := TCnIntegerList.Create;
  FHosts := TList.Create;
  FSelectedHost := nil;
{$IFNDEF SUPPORT_MARGIN_PADDING}
  FPadding := TCnPadding.Create;
  FPadding.OnChange := PaddingChanged;
{$ENDIF}
  FPageChars := 65536;
  FDefaultHeight := 24;
  FItemSpacing := 4;
  FHostBufferItems := 3;
  FPendingMessage := -2;
  FQueuedMessage := -1;
  FPendingFinishMessage := -1;
  FFlushTimer := TTimer.Create(Self);
  FFlushTimer.Interval := 33;
  FFlushTimer.Enabled := False;
  FFlushTimer.OnTimer := FlushTimer;
{$IFDEF SUPPORT_MARGIN_PADDING}
  Padding.SetBounds(8, 8, 8, 8);
{$ENDIF}
  FLastPaddingLeft := GetPaddingLeft;
  FLastPaddingTop := GetPaddingTop;
  FLastPaddingRight := GetPaddingRight;
  FLastPaddingBottom := GetPaddingBottom;
  { 构造期设置默认留白不应留下待处理的重建任务。 }
  FPendingMessage := -2;
  FFlushTimer.Enabled := False;
end;

destructor TCnMarkDownView.Destroy;
begin
  if FFeed <> nil then
    FFeed.OnChange := nil;
  ClearHosts;
  FHosts.Free;
  FItems.Free;
  FMessageStarts.Free;
  FIndex.Free;
{$IFNDEF SUPPORT_MARGIN_PADDING}
  FPadding.Free;
{$ENDIF}
  inherited Destroy;
end;

procedure TCnMarkDownView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_VSCROLL or WS_CLIPCHILDREN;
end;

{$IFDEF SUPPORT_MARGIN_PADDING}

procedure TCnMarkDownView.AlignControls(AControl: TControl;
  var Rect: TRect);
begin
  inherited AlignControls(AControl, Rect);
  { 新版 VCL 的 Padding 变化会触发子控件重新对齐，在这里同步虚拟布局。 }
  CheckPaddingChanged;
end;

{$ENDIF}

function TCnMarkDownView.GetPaddingLeft: Integer;
begin
{$IFDEF SUPPORT_MARGIN_PADDING}
  Result := Padding.Left;
{$ELSE}
  Result := FPadding.Left;
{$ENDIF}
  if Result < 0 then
    Result := 0;
end;

function TCnMarkDownView.GetPaddingTop: Integer;
begin
{$IFDEF SUPPORT_MARGIN_PADDING}
  Result := Padding.Top;
{$ELSE}
  Result := FPadding.Top;
{$ENDIF}
  if Result < 0 then
    Result := 0;
end;

function TCnMarkDownView.GetPaddingRight: Integer;
begin
{$IFDEF SUPPORT_MARGIN_PADDING}
  Result := Padding.Right;
{$ELSE}
  Result := FPadding.Right;
{$ENDIF}
  if Result < 0 then
    Result := 0;
end;

function TCnMarkDownView.GetPaddingBottom: Integer;
begin
{$IFDEF SUPPORT_MARGIN_PADDING}
  Result := Padding.Bottom;
{$ELSE}
  Result := FPadding.Bottom;
{$ENDIF}
  if Result < 0 then
    Result := 0;
end;

function TCnMarkDownView.GetContentWidth: Integer;
var
  W: Int64;
begin
  W := Int64(ClientWidth) - Int64(GetPaddingLeft) -
    Int64(GetPaddingRight);
  if W < 1 then
    Result := 1
  else if W > MaxInt then
    Result := MaxInt
  else
    Result := Integer(W);
end;

function TCnMarkDownView.ResolveItemBackgroundColor(
  Item: TCnMarkDownViewItem): TColor;
begin
  Result := FBackgroundColor;
  if (Item <> nil) and Odd(Item.MessageIndex) then
    Result := FAlternateItemColor;
  if Result = clNone then
    Result := FBackgroundColor;
  if Assigned(FOnGetItemBackgroundColor) then
    FOnGetItemBackgroundColor(Self, Item, Result);
  if Result = clNone then
    Result := FBackgroundColor;
end;

procedure TCnMarkDownView.CheckPaddingChanged;
var
  L, T, R, B: Integer;
begin
  L := GetPaddingLeft;
  T := GetPaddingTop;
  R := GetPaddingRight;
  B := GetPaddingBottom;
  if (L = FLastPaddingLeft) and (T = FLastPaddingTop) and
    (R = FLastPaddingRight) and (B = FLastPaddingBottom) then
    Exit;
  FLastPaddingLeft := L;
  FLastPaddingTop := T;
  FLastPaddingRight := R;
  FLastPaddingBottom := B;
  { 左右留白会改变折行宽度，因此统一重建估算高度和可见宿主。 }
  if (FIndex <> nil) and (FFlushTimer <> nil) then
    SetPendingMessage(-1);
  if HandleAllocated then
    Invalidate;
end;

{$IFNDEF SUPPORT_MARGIN_PADDING}

procedure TCnMarkDownView.SetPadding(Value: TCnPadding);
begin
  if Value <> nil then
    FPadding.Assign(Value);
end;

procedure TCnMarkDownView.PaddingChanged(Sender: TObject);
begin
  CheckPaddingChanged;
end;

{$ENDIF}

procedure TCnMarkDownView.SetFeed(Value: TCnMarkDownFeed);
begin
  if FFeed = Value then
    Exit;
  if FFeed <> nil then
    FFeed.OnChange := nil;
  FFeed := Value;
  if FFeed <> nil then
    FFeed.OnChange := FeedChanged;
  FPendingMessage := -1;
  FQueuedMessage := -1;
  FPendingFinishMessage := -1;
  Flush;
end;

procedure TCnMarkDownView.SetPageChars(Value: Integer);
begin
  if Value < 1024 then
    Value := 1024;
  if FPageChars <> Value then
  begin
    FPageChars := Value;
    SetPendingMessage(-1);
  end;
end;

procedure TCnMarkDownView.SetDefaultHeight(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if FDefaultHeight <> Value then
  begin
    FDefaultHeight := Value;
    SetPendingMessage(-1);
  end;
end;

procedure TCnMarkDownView.SetItemSpacing(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if FItemSpacing <> Value then
  begin
    FItemSpacing := Value;
    SetPendingMessage(-1);
  end;
end;

procedure TCnMarkDownView.SetBackgroundColor(Value: TColor);
begin
  if FBackgroundColor = Value then
    Exit;
  FBackgroundColor := Value;
  RefreshItemBackgroundColors;
end;

procedure TCnMarkDownView.SetAlternateItemColor(Value: TColor);
begin
  if FAlternateItemColor = Value then
    Exit;
  FAlternateItemColor := Value;
  RefreshItemBackgroundColors;
end;

procedure TCnMarkDownView.SetOnGetItemBackgroundColor(
  Value: TCnMarkDownGetItemBackgroundColorEvent);
begin
  FOnGetItemBackgroundColor := Value;
  RefreshItemBackgroundColors;
end;

procedure TCnMarkDownView.RefreshItemBackgroundColors;
var
  I: Integer;
  Item: TCnMarkDownViewItem;
  Host: TCnMarkDownRichHost;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    Item := TCnMarkDownViewItem(FItems[I]);
    Item.BackgroundColor := ResolveItemBackgroundColor(Item);
  end;
  for I := 0 to FHosts.Count - 1 do
  begin
    Host := TCnMarkDownRichHost(FHosts[I]);
    if (Host.ItemIndex >= 0) and (Host.ItemIndex < FItems.Count) then
      Host.Color := TCnMarkDownViewItem(
        FItems[Host.ItemIndex]).BackgroundColor
    else
      Host.Color := FBackgroundColor;
  end;
  Invalidate;
end;

procedure TCnMarkDownView.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCnMarkDownView.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    if FPendingMessage <> -2 then
      Flush;
    if (FQueuedMessage >= 0) or (FPendingFinishMessage >= 0) then
      FFlushTimer.Enabled := True;
  end;
end;

procedure TCnMarkDownView.SetPendingMessage(MessageIndex: Integer);
begin
  if MessageIndex < 0 then
    FPendingMessage := -1
  else if (FPendingMessage = -2) or (MessageIndex < FPendingMessage) then
    FPendingMessage := MessageIndex;
  if (FUpdateCount = 0) and (FPendingMessage <> -2) then
    FFlushTimer.Enabled := True;
end;

procedure TCnMarkDownView.FeedChanged(Sender: TObject;
  ChangeType: TCnMarkDownFeedChangeType; MessageIndex: Integer);
begin
  if ChangeType = cmfMessageQueued then
  begin
    if (FQueuedMessage < 0) or (MessageIndex < FQueuedMessage) then
      FQueuedMessage := MessageIndex;
    if FUpdateCount = 0 then
      FFlushTimer.Enabled := True;
  end
  else if ChangeType in [cmfReset, cmfBatchChanged] then
  begin
    if ChangeType = cmfReset then
    begin
      FQueuedMessage := -1;
      FPendingFinishMessage := -1;
    end;
    SetPendingMessage(-1)
  end
  else
    SetPendingMessage(MessageIndex);
end;

procedure TCnMarkDownView.FlushTimer(Sender: TObject);
var
  I, N: Integer;
  Started: Cardinal;
begin
  if FFeed <> nil then
  begin
    I := FQueuedMessage;
    Started := GetTickCount;
    while (I >= 0) and (I < FFeed.MessageCount) and
      ((GetTickCount - Started) < 8) do
    begin
      N := FFeed.FlushMessageQueue(I, 32768);
      if FFeed.Messages[I].PendingLength > 0 then
        Break;
      if N = 0 then
        Inc(I)
      else
        Inc(I);
    end;
    if (I >= 0) and (I < FFeed.MessageCount) and
      (FFeed.Messages[I].PendingLength > 0) then
      FQueuedMessage := I
    else
      FQueuedMessage := -1;
    if (FPendingFinishMessage >= 0) and
      (FPendingFinishMessage < FFeed.MessageCount) and
      (FFeed.Messages[FPendingFinishMessage].PendingLength = 0) then
    begin
      FFeed.FinishMessage(FPendingFinishMessage);
      FPendingFinishMessage := -1;
    end;
  end;
  Flush;
  if (FQueuedMessage >= 0) or (FPendingFinishMessage >= 0) then
    FFlushTimer.Enabled := True;
end;

procedure TCnMarkDownView.Flush;
var
  Dirty: Integer;
begin
  if FUpdateCount > 0 then
    Exit;
  { 原生 TPadding 的修改通过对齐消息异步通知，这里再检查一次确保布局及时更新。 }
  CheckPaddingChanged;
  FFlushTimer.Enabled := False;
  Dirty := FPendingMessage;
  FPendingMessage := -2;
  if Dirty = -2 then
    Exit;
  if Dirty < 0 then
    RebuildAll
  else
    RebuildFromMessage(Dirty);
end;

procedure TCnMarkDownView.ClearHosts;
var
  I: Integer;
  Host: TCnMarkDownRichHost;
begin
  FSelectedHost := nil;
  for I := FHosts.Count - 1 downto 0 do
  begin
    Host := TCnMarkDownRichHost(FHosts[I]);
    ReleaseHostFocus(Host);
    Host.Free;
  end;
  FHosts.Clear;
end;

procedure TCnMarkDownView.EnsureHosts;
var
  NeedCount, I: Integer;
  Host: TCnMarkDownRichHost;
begin
  NeedCount := (ClientHeight div FDefaultHeight) + FHostBufferItems + 5;
  if NeedCount < 4 then
    NeedCount := 4;
  while FHosts.Count < NeedCount do
  begin
    Host := TCnMarkDownRichHost.Create(Self);
    Host.Parent := Self;
    Host.View := Self;
    Host.ItemIndex := -1;
    Host.MessageIndex := -1;
    Host.BlockIndex := -1;
    Host.BlockID := 0;
    Host.BlockRevision := 0;
    Host.DocumentRevision := 0;
    Host.BorderStyle := bsNone;
    Host.ScrollBars := ssNone;
    Host.WordWrap := True;
    { 虚拟宿主的高度由布局管理，不能让 TRichEdit 自动改回单行高度。 }
    Host.AutoSize := False;
    Host.ReadOnly := True;
    Host.HideSelection := False;
    Host.TabStop := False;
    Host.Color := FBackgroundColor;
    Host.Visible := False;
    Host.Perform(EM_SETEVENTMASK, 0,
      Host.Perform(EM_GETEVENTMASK, 0, 0) or ENM_REQUESTRESIZE);
    FHosts.Add(Host);
  end;
  while FHosts.Count > NeedCount do
  begin
    Host := TCnMarkDownRichHost(FHosts[FHosts.Count - 1]);
    if FSelectedHost = Host then
      FSelectedHost := nil;
    ReleaseHostFocus(Host);
    Host.Free;
    FHosts.Delete(FHosts.Count - 1);
  end;
  for I := 0 to FHosts.Count - 1 do
    TCnMarkDownRichHost(FHosts[I]).Width := GetContentWidth;
end;

procedure TCnMarkDownView.ReorderHosts(StartIndex: Integer);
var
  I, J, FoundIndex, ItemIndex, LastIndex: Integer;
  Host: TCnMarkDownRichHost;
begin
  LastIndex := StartIndex + FHosts.Count - 1;
  for I := 0 to FHosts.Count - 1 do
  begin
    ItemIndex := StartIndex + I;
    FoundIndex := -1;
    for J := I to FHosts.Count - 1 do
      if TCnMarkDownRichHost(FHosts[J]).ItemIndex = ItemIndex then
      begin
        FoundIndex := J;
        Break;
      end;
    if FoundIndex < 0 then
      for J := FHosts.Count - 1 downto I do
      begin
        Host := TCnMarkDownRichHost(FHosts[J]);
        if (Host.ItemIndex < StartIndex) or (Host.ItemIndex > LastIndex) then
        begin
          FoundIndex := J;
          Break;
        end;
      end;
    if (FoundIndex >= 0) and (FoundIndex <> I) then
      FHosts.Move(FoundIndex, I);
  end;
end;

procedure TCnMarkDownView.RebuildIndex;
var
  I: Integer;
begin
  FIndex.SetCount(FItems.Count, FDefaultHeight);
  for I := 0 to FItems.Count - 1 do
    FIndex.Heights[I] := TCnMarkDownViewItem(FItems[I]).Height + FItemSpacing;
end;

procedure TCnMarkDownView.RebuildAll;
var
  I: Integer;
  OldOffset, OldMax: Int64;
  AtBottom: Boolean;
begin
  OldOffset := FScrollOffset;
  OldMax := MaxScroll;
  AtBottom := (FItems.Count = 0) or (OldOffset >= OldMax - 2);
  FItems.Clear;
  if FFeed = nil then
    FMessageStarts.Clear
  else
  begin
    FMessageStarts.Count := FFeed.MessageCount + 1;
    for I := 0 to FFeed.MessageCount - 1 do
    begin
      FMessageStarts[I] := FItems.Count;
      AddMessageItems(I);
    end;
    FMessageStarts[FFeed.MessageCount] := FItems.Count;
  end;
  RebuildIndex;
  if AtBottom then
    FScrollOffset := MaxScroll
  else
    FScrollOffset := ScrollToPosition(OldOffset);
  { 视口位置确定后再同步滚动条，保证二者使用同一个偏移量。 }
  UpdateScrollBar;
  ClearHosts;
  EnsureHosts;
  LayoutHosts;
  Invalidate;
end;

procedure TCnMarkDownView.RebuildFromMessage(MessageIndex: Integer);
var
  I, StartIndex, OldEndIndex, OldItemCount, AppendStart,
  NewItemCount, Delta, OldAnchor, OldAnchorInner, NewAnchor: Integer;
  OldMessageIndex, OldBlockIndex, OldTextStart: Integer;
  OldOffset, OldMax: Int64;
  OldItem: TCnMarkDownViewItem;
  Host: TCnMarkDownRichHost;
  AtBottom: Boolean;
begin
  if (FFeed = nil) or (MessageIndex < 0) or
    (MessageIndex >= FFeed.MessageCount) then
  begin
    RebuildAll;
    Exit;
  end;

  OldOffset := FScrollOffset;
  OldMax := MaxScroll;
  AtBottom := OldOffset >= OldMax - 2;
  OldAnchor := FIndex.IndexAtOffset(OldOffset - GetPaddingTop);
  OldAnchorInner := 0;
  OldMessageIndex := -1;
  OldBlockIndex := -1;
  OldTextStart := -1;
  if (OldAnchor >= 0) and (OldAnchor < FItems.Count) then
  begin
    OldItem := TCnMarkDownViewItem(FItems[OldAnchor]);
    OldMessageIndex := OldItem.MessageIndex;
    OldBlockIndex := OldItem.BlockIndex;
    OldTextStart := OldItem.TextStart;
    OldAnchorInner := Integer(OldOffset - GetPaddingTop -
      FIndex.TopOf(OldAnchor));
  end;

  if FMessageStarts.Count <> FFeed.MessageCount + 1 then
    RebuildAll
  else
  begin
    StartIndex := FMessageStarts[MessageIndex];
    OldEndIndex := FMessageStarts[MessageIndex + 1];
    OldItemCount := OldEndIndex - StartIndex;

    { 先在列表尾部构造变化消息的新条目，失败时可完整保留原布局。 }
    AppendStart := FItems.Count;
    try
      AddMessageItems(MessageIndex);
    except
      for I := FItems.Count - 1 downto AppendStart do
        FItems.Delete(I);
      raise;
    end;
    NewItemCount := FItems.Count - AppendStart;

    { 仅替换变化消息的范围，保留下游条目已经测得的准确高度。 }
    for I := OldEndIndex - 1 downto StartIndex do
      FItems.Delete(I);
    Dec(AppendStart, OldItemCount);
    for I := 0 to NewItemCount - 1 do
      FItems.Move(AppendStart + I, StartIndex + I);

    Delta := NewItemCount - OldItemCount;
    if Delta <> 0 then
    begin
      { 下游宿主仍绑定同一条目，只修正虚拟索引即可避免无谓重载和误测高。 }
      for I := 0 to FHosts.Count - 1 do
      begin
        Host := TCnMarkDownRichHost(FHosts[I]);
        if Host.ItemIndex >= OldEndIndex then
          Host.ItemIndex := Host.ItemIndex + Delta;
      end;
      for I := MessageIndex + 1 to FFeed.MessageCount do
        FMessageStarts[I] := FMessageStarts[I] + Delta;
    end;
    RebuildIndex;

    if AtBottom then
      FScrollOffset := MaxScroll
    else if OldMessageIndex >= 0 then
    begin
      NewAnchor := FindItem(OldMessageIndex, OldBlockIndex, OldTextStart);
      if NewAnchor >= 0 then
        FScrollOffset := ScrollToPosition(GetPaddingTop +
          FIndex.TopOf(NewAnchor) + OldAnchorInner)
      else
        FScrollOffset := ScrollToPosition(OldOffset);
    end
    else
      FScrollOffset := ScrollToPosition(OldOffset);
    { 锚点位置确定后再同步滚动条。 }
    UpdateScrollBar;
    EnsureHosts;
    LayoutHosts;
    Invalidate;
  end;
end;

procedure TCnMarkDownView.AddMessageItems(MessageIndex: Integer);
var
  I: Integer;
begin
  if (FFeed = nil) or (MessageIndex < 0) or
    (MessageIndex >= FFeed.MessageCount) then
    Exit;
  for I := 0 to FFeed.Messages[MessageIndex].Document.BlockCount - 1 do
    AddBlockItems(MessageIndex, I);
end;

function TCnMarkDownView.FindPageLength(const Text: TCnMarkDownText;
  StartPos: Integer): Integer;
var
  Limit, I: Integer;
begin
  Limit := Length(Text) - StartPos;
  if Limit > FPageChars then
    Limit := FPageChars;
  if Limit <= 0 then
  begin
    Result := 0;
    Exit;
  end;
  if (StartPos + Limit < Length(Text)) then
  begin
    I := Limit;
    while (I > 1024) and (Text[StartPos + I] <> #10) do
      Dec(I);
    if I > 1024 then
      Limit := I;
  end;
  Result := Limit;
end;

function TCnMarkDownView.EstimateHeight(Block: TCnMarkDownBlock;
  StartPos, TextLength: Integer): Integer;
var
  I, CharWidth, LineChars, CurrentChars, Lines: Integer;
begin
  Canvas.Font.Assign(Font);
  CharWidth := Canvas.TextWidth('W');
  if CharWidth < 1 then
    CharWidth := 8;
  LineChars := (GetContentWidth - 20) div CharWidth;
  if LineChars < 8 then
    LineChars := 8;
  Lines := 0;
  CurrentChars := 0;
  for I := 0 to TextLength - 1 do
  begin
    if Block.Text[StartPos + I + 1] = #10 then
    begin
      if CurrentChars = 0 then
        Inc(Lines)
      else
        Inc(Lines, (CurrentChars + LineChars - 1) div LineChars);
      CurrentChars := 0;
    end
    else
      Inc(CurrentChars);
  end;
  if CurrentChars = 0 then
  begin
    if Lines = 0 then
      Lines := 1;
  end
  else
    Inc(Lines, (CurrentChars + LineChars - 1) div LineChars);
  Result := Lines * LineHeight;
  if Block.BlockType = cmbHeading then
    Inc(Result, 6)
  else if Block.BlockType = cmbCodeBlock then
    Inc(Result, 8);
  if Result < FDefaultHeight then
    Result := FDefaultHeight;
end;

function TCnMarkDownView.AddBlockItems(MessageIndex, BlockIndex: Integer): Integer;
var
  Block: TCnMarkDownBlock;
  Item: TCnMarkDownViewItem;
  StartPos, PageLength: Integer;
begin
  Result := 0;
  Block := FFeed.Messages[MessageIndex].Document.Blocks[BlockIndex];
  StartPos := 0;

  while StartPos < Length(Block.Text) do
  begin
    PageLength := FindPageLength(Block.Text, StartPos);
    if PageLength <= 0 then
      Break;
    Item := TCnMarkDownViewItem.Create;
    Item.MessageIndex := MessageIndex;
    Item.BlockIndex := BlockIndex;
    Item.BlockID := Block.BlockID;
    Item.Revision := Block.Revision;
    Item.DocumentRevision := FFeed.Messages[MessageIndex].Document.Revision;
    Item.TextStart := StartPos;
    Item.TextLength := PageLength;
    Item.Height := EstimateHeight(Block, StartPos, PageLength);
    Item.BackgroundColor := ResolveItemBackgroundColor(Item);
    FItems.Add(Item);
    Inc(Result);
    Inc(StartPos, PageLength);
  end;
  if Length(Block.Text) = 0 then
  begin
    Item := TCnMarkDownViewItem.Create;
    Item.MessageIndex := MessageIndex;
    Item.BlockIndex := BlockIndex;
    Item.BlockID := Block.BlockID;
    Item.Revision := Block.Revision;
    Item.DocumentRevision := FFeed.Messages[MessageIndex].Document.Revision;
    Item.TextStart := 0;
    Item.TextLength := 0;
    Item.Height := EstimateHeight(Block, 0, 0);
    Item.BackgroundColor := ResolveItemBackgroundColor(Item);
    FItems.Add(Item);
    Inc(Result);
  end;
end;

function TCnMarkDownView.FindItem(MessageIndex, BlockIndex,
  TextStart: Integer): Integer;
var
  I: Integer;
  Item: TCnMarkDownViewItem;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    Item := TCnMarkDownViewItem(FItems[I]);
    if (Item.MessageIndex = MessageIndex) and
      (Item.BlockIndex = BlockIndex) and (Item.TextStart = TextStart) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function TCnMarkDownView.MaxScroll: Int64;
begin
  Result := GetTotalHeight - ClientHeight;
  if Result < 0 then
    Result := 0;
end;

function TCnMarkDownView.GetTotalHeight: Int64;
begin
  Result := Int64(GetPaddingTop) + FIndex.TotalHeight +
    Int64(GetPaddingBottom);
end;

function TCnMarkDownView.GetItemCount: Integer;
begin
  Result := FIndex.Count;
end;

function TCnMarkDownView.ScrollToPosition(Value: Int64): Int64;
begin
  if Value < 0 then
    Value := 0;
  if Value > MaxScroll then
    Value := MaxScroll;
  Result := Value;
end;

function TCnMarkDownView.ScrollFromPosition(Value: Integer): Int64;
var
  Total, MaxPos, TrackMax: Int64;
  Page: LongWord;
  ScaledValue: Extended;
begin
  Total := GetTotalHeight;
  if (Total <= MaxInt) then
    Result := Value
  else if (Value <= 0) or (MaxScroll <= 0) then
    Result := 0
  else
  begin
    MaxPos := MaxScroll;
    { 大内容量时，滚动条的有效位置范围还要扣除页面大小。 }
    ScaledValue := ClientHeight;
    ScaledValue := ScaledValue / Total;
    ScaledValue := ScaledValue * MaxInt;
    Page := LongWord(Round(ScaledValue));
    if Page < 1 then
      Page := 1;
    TrackMax := Int64(MaxInt) - Int64(Page) + 1;
    if TrackMax < 1 then
      TrackMax := 1;
    if Int64(Value) > TrackMax then
      Value := Integer(TrackMax);
    ScaledValue := Value;
    ScaledValue := ScaledValue / TrackMax;
    ScaledValue := ScaledValue * MaxPos;
    Result := Round(ScaledValue);
  end;
  Result := ScrollToPosition(Result);
end;

procedure TCnMarkDownView.UpdateScrollBar;
var
  SI: TScrollInfo;
  Total, MaxPos, TrackMax: Int64;
  ScaledValue: Extended;
begin
  if not HandleAllocated then
    Exit;
  Total := GetTotalHeight;
  MaxPos := MaxScroll;
  SI.cbSize := SizeOf(SI);
  SI.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
  SI.nMin := 0;
  if Total <= MaxInt then
  begin
    { nMax 是内容最后一个像素，保证有效位置正好等于 Total-ClientHeight。 }
    if Total > 0 then
      SI.nMax := Integer(Total - 1)
    else
      SI.nMax := 0;
    SI.nPage := ClientHeight;
    SI.nPos := Integer(FScrollOffset);
  end
  else
  begin
    SI.nMax := MaxInt;
    ScaledValue := ClientHeight;
    ScaledValue := ScaledValue / Total;
    ScaledValue := ScaledValue * MaxInt;
    SI.nPage := LongWord(Round(ScaledValue));
    if SI.nPage < 1 then
      SI.nPage := 1;
    TrackMax := Int64(SI.nMax) - Int64(SI.nPage) + 1;
    if TrackMax < 1 then
      TrackMax := 1;
    if MaxPos <= 0 then
      SI.nPos := 0
    else
    begin
      ScaledValue := FScrollOffset;
      ScaledValue := ScaledValue / MaxPos;
      ScaledValue := ScaledValue * TrackMax;
      SI.nPos := Integer(Round(ScaledValue));
    end;
  end;
  TrackMax := Int64(SI.nMax) - Int64(SI.nPage) + 1;
  if TrackMax < 0 then
    TrackMax := 0;
  if SI.nPos < 0 then
    SI.nPos := 0;
  if Int64(SI.nPos) > TrackMax then
    SI.nPos := Integer(TrackMax);
  SetScrollInfo(Handle, SB_VERT, SI, True);
end;

function TCnMarkDownView.LineHeight: Integer;
begin
  Canvas.Font.Assign(Font);
  Result := Canvas.TextHeight('W') + 4;
  if Result < 16 then
    Result := 16;
end;

procedure TCnMarkDownView.UpdateItemHeight(ItemIndex, Value: Integer);
var
  Item: TCnMarkDownViewItem;
  OldOffset, OldMax: Int64;
begin
  if (ItemIndex < 0) or (ItemIndex >= FItems.Count) then
    Exit;
  if Value < FDefaultHeight then
    Value := FDefaultHeight;
  Item := TCnMarkDownViewItem(FItems[ItemIndex]);
  if Item.Height = Value then
    Exit;
  OldOffset := FScrollOffset;
  OldMax := MaxScroll;
  Item.Height := Value;
  FIndex.Heights[ItemIndex] := Value + FItemSpacing;
  if (OldOffset >= OldMax - 2) then
    FScrollOffset := MaxScroll
  else
    FScrollOffset := ScrollToPosition(OldOffset);
  UpdateScrollBar;
  FHeightChanged := True;
end;

procedure TCnMarkDownView.LoadHostBlock(Host: TCnMarkDownRichHost;
  Item: TCnMarkDownViewItem);
var
  Block, PageBlock: TCnMarkDownBlock;
  Rtf: AnsiString;
  Stream: TMemoryStream;
begin
  Block := FFeed.Messages[Item.MessageIndex].Document.Blocks[Item.BlockIndex];
  if (Item.TextStart = 0) and (Item.TextLength = Length(Block.Text)) and
    (Block.InlineCount > 0) then
    Rtf := CnMarkDownBlockToUnicodeRTF(Block, 10, True)
  else
  begin
    PageBlock := TCnMarkDownBlock.Create;
    try
      PageBlock.BlockType := Block.BlockType;
      PageBlock.HeadingLevel := Block.HeadingLevel;
      PageBlock.QuoteLevel := Block.QuoteLevel;
      PageBlock.ListLevel := Block.ListLevel;
      PageBlock.ListStart := Block.ListStart;
      PageBlock.CodeLanguage := Block.CodeLanguage;
      PageBlock.Text := Copy(Block.Text, Item.TextStart + 1, Item.TextLength);
      PageBlock.RebuildInlines;
      Rtf := CnMarkDownBlockToUnicodeRTF(PageBlock, 10, True);
    finally
      PageBlock.Free;
    end;
  end;
  Stream := TMemoryStream.Create;
  try
    if Length(Rtf) > 0 then
      Stream.WriteBuffer(Rtf[1], Length(Rtf));
    Stream.Position := 0;
    Host.Lines.BeginUpdate;
    try
      { RichEdit 的流入实现依赖具体版本，显式清空可避免复用宿主时叠加旧文本。 }
      Host.ReadOnly := False;
      try
        Host.Clear;
        Host.Lines.LoadFromStream(Stream);
      finally
        Host.ReadOnly := True;
      end;
    finally
      Host.Lines.EndUpdate;
    end;
  finally
    Stream.Free;
  end;
  Host.Modified := False;
end;

procedure TCnMarkDownView.ActivateHost(Host: TCnMarkDownRichHost);
var
  I: Integer;
  Other: TCnMarkDownRichHost;
begin
  { 每个可见分页都是独立的 RichEdit，因此需要在控件层统一选择状态。 }
  for I := 0 to FHosts.Count - 1 do
  begin
    Other := TCnMarkDownRichHost(FHosts[I]);
    if (Other <> Host) and Other.HandleAllocated and
      (Other.SelLength <> 0) then
      Other.SelLength := 0;
  end;
  if (Host <> nil) and (Host.ItemIndex >= 0) then
    FSelectedHost := Host;
end;

function TCnMarkDownView.GetSelectedItemIndex: Integer;
var
  I: Integer;
  Host: TCnMarkDownRichHost;
begin
  Result := -1;
  { 优先返回有文本选择的宿主，按钮获得焦点后仍能识别用户刚选中的条目。 }
  for I := 0 to FHosts.Count - 1 do
  begin
    Host := TCnMarkDownRichHost(FHosts[I]);
    if (Host.ItemIndex >= 0) and (Host.ItemIndex < FItems.Count) and
      (Host.SelLength <> 0) then
    begin
      Result := Host.ItemIndex;
      Exit;
    end;
  end;
  { 没有选中文本时，使用最后获得焦点的可见宿主作为当前条目。 }
  if (FSelectedHost <> nil) and FSelectedHost.Visible and
    (FSelectedHost.ItemIndex >= 0) and
    (FSelectedHost.ItemIndex < FItems.Count) then
    Result := FSelectedHost.ItemIndex;
end;

function TCnMarkDownView.GetSelectedMessageIndex: Integer;
var
  ItemIndex: Integer;
begin
  ItemIndex := GetSelectedItemIndex;
  if (ItemIndex >= 0) and (ItemIndex < FItems.Count) then
    Result := TCnMarkDownViewItem(FItems[ItemIndex]).MessageIndex
  else
    Result := -1;
end;

procedure TCnMarkDownView.ReleaseHostFocus(Host: TCnMarkDownRichHost);
begin
  { 隐藏或释放拥有焦点的 RichEdit 会使后续滚轮消息丢失目标。 }
  if (Host <> nil) and Host.HandleAllocated and
    (Windows.GetFocus = Host.Handle) and
    not (csDestroying in ComponentState) and CanFocus then
    SetFocus;
end;

procedure TCnMarkDownView.SetHostFormatRect(Host: TCnMarkDownRichHost);
var
  FormatRect: TRect;
begin
  FormatRect := Host.ClientRect;
  if FormatRect.Right > FormatRect.Left + 2 then
  begin
    Inc(FormatRect.Left);
    Dec(FormatRect.Right);
  end;
  if FormatRect.Bottom > FormatRect.Top + 2 then
    Inc(FormatRect.Top);
  { 保留底部完整客户区，避免最后一行的字形下沿被裁掉。 }
  { 显式传入矩形，确保宿主换页后格式区域与当前客户区一致。 }
  Host.Perform(EM_SETRECT, 0, LPARAM(@FormatRect));
end;

procedure TCnMarkDownView.PrepareHostMeasure(Host: TCnMarkDownRichHost);
begin
  { 先给隐藏宿主足够大的临时高度，避免小格式矩形裁掉代码块末行。 }
  Host.SetBounds(GetPaddingLeft, 0, GetContentWidth, 32767);
  SetHostFormatRect(Host);
end;

function TCnMarkDownView.ValidateHostHeight(Host: TCnMarkDownRichHost;
  Item: TCnMarkDownViewItem; Value: Integer): Integer;
var
  Block: TCnMarkDownBlock;
  DisplayLines, BaseLine, TextHeightEstimate, FallbackHeight: Integer;
begin
  DisplayLines := Integer(Host.Perform(EM_GETLINECOUNT, 0, 0));
  if DisplayLines < 1 then
    DisplayLines := 1;
  BaseLine := LineHeight;

  { EM_GETLINECOUNT 包含自动折行，用它生成不依赖旧窗口高度的后备值。 }
  Block := FFeed.Messages[Item.MessageIndex].Document.Blocks[Item.BlockIndex];
  if Block.BlockType = cmbHeading then
    FallbackHeight := DisplayLines * BaseLine * 2 + 8
  else if Block.BlockType = cmbCodeBlock then
    FallbackHeight := DisplayLines * BaseLine + 8
  else
    FallbackHeight := DisplayLines * BaseLine + 6;
  { RichEdit 对 \line 的行数回报可能偏小，再用当前虚拟页文本补充下限。 }
  TextHeightEstimate := EstimateHeight(Block, Item.TextStart, Item.TextLength);
  Inc(TextHeightEstimate, 6);
  if TextHeightEstimate > FallbackHeight then
    FallbackHeight := TextHeightEstimate;
  if FallbackHeight < FDefaultHeight then
    FallbackHeight := FDefaultHeight;

  { 测量区域已在请求前清空旧高度，因此这里不再截断合法的较大回报值。 }
  if (Value <= 0) or (Value < FallbackHeight) then
    Result := FallbackHeight
  else
    Result := Value;
end;

procedure TCnMarkDownView.BindHost(Host: TCnMarkDownRichHost;
  ItemIndex: Integer);
var
  Item: TCnMarkDownViewItem;
  MeasuredHeight: Integer;
begin
  if (ItemIndex < 0) or (ItemIndex >= FItems.Count) then
  begin
    if FSelectedHost = Host then
      FSelectedHost := nil;
    ReleaseHostFocus(Host);
    Host.Visible := False;
    if Host.HandleAllocated and (Host.SelLength <> 0) then
      Host.SelLength := 0;
    Host.ItemIndex := -1;
    Host.MessageIndex := -1;
    Host.BlockIndex := -1;
    Host.BlockID := 0;
    Host.BlockRevision := 0;
    Host.DocumentRevision := 0;
    Exit;
  end;
  Item := TCnMarkDownViewItem(FItems[ItemIndex]);
  if Host.Color <> Item.BackgroundColor then
    Host.Color := Item.BackgroundColor;
  if (Host.ItemIndex <> ItemIndex) or
    (Host.MessageIndex <> Item.MessageIndex) or
    (Host.BlockIndex <> Item.BlockIndex) or
    (Host.BlockID <> Item.BlockID) or
    (Host.BlockRevision <> Item.Revision) or
    (Host.DocumentRevision <> Item.DocumentRevision) then
  begin
    if FSelectedHost = Host then
      FSelectedHost := nil;
    { 重新绑定的宿主在内容和高度稳定前不参与屏幕绘制。 }
    ReleaseHostFocus(Host);
    Host.Visible := False;
    if Host.HandleAllocated and (Host.SelLength <> 0) then
      Host.SelLength := 0;
    Host.ItemIndex := ItemIndex;
    Host.MessageIndex := Item.MessageIndex;
    Host.BlockIndex := Item.BlockIndex;
    Host.BlockID := Item.BlockID;
    Host.BlockRevision := Item.Revision;
    Host.DocumentRevision := Item.DocumentRevision;
    PrepareHostMeasure(Host);
    LoadHostBlock(Host, Item);
    { 载入 RTF 可能重置格式区域，必须在请求高度前再次同步当前客户区。 }
    SetHostFormatRect(Host);
    FMeasureHost := Host;
    FRequestedHeight := 0;
    try
      Host.Perform(EM_REQUESTRESIZE, 0, 0);
    finally
      FMeasureHost := nil;
    end;
    MeasuredHeight := ValidateHostHeight(Host, Item, FRequestedHeight);
    UpdateItemHeight(ItemIndex, MeasuredHeight);
  end;
end;

procedure TCnMarkDownView.LayoutHosts;
var
  StartIndex, I, ItemIndex, LayoutPass, MaxLayoutPasses: Integer;
  Y, ContentOffset: Int64;
  H, ContentLeft, ContentWidth: Integer;
  Host: TCnMarkDownRichHost;
begin
  if FInLayout then
    Exit;
  FInLayout := True;
  try
    EnsureHosts;
    { 每次高度变化至少测得一个新宿主，以宿主数限制循环即可避免递归。 }
    MaxLayoutPasses := FHosts.Count + 1;
    if MaxLayoutPasses < 4 then
      MaxLayoutPasses := 4;
    LayoutPass := 0;
    repeat
      FHeightChanged := False;
      ContentOffset := FScrollOffset - GetPaddingTop;
      StartIndex := FIndex.IndexAtOffset(ContentOffset);
      if StartIndex < 0 then
      begin
        for I := 0 to FHosts.Count - 1 do
        begin
          Host := TCnMarkDownRichHost(FHosts[I]);
          ReleaseHostFocus(Host);
          Host.Visible := False;
        end;
        Break;
      end;
      { 先按虚拟项重排宿主，滚动一项时通常只需重新载入一个宿主。 }
      ReorderHosts(StartIndex);
      ContentLeft := GetPaddingLeft;
      ContentWidth := GetContentWidth;
      Y := GetPaddingTop + FIndex.TopOf(StartIndex) - FScrollOffset;
      for I := 0 to FHosts.Count - 1 do
      begin
        ItemIndex := StartIndex + I;
        Host := TCnMarkDownRichHost(FHosts[I]);
        if (ItemIndex >= 0) and (ItemIndex < FItems.Count) then
        begin
          if (Y > ClientHeight + FHostBufferItems * FDefaultHeight) and
            (I > 0) then
          begin
            BindHost(Host, -1);
            Continue;
          end;
          BindHost(Host, ItemIndex);
          { BindHost 可能通过 EN_REQUESTRESIZE 修正高度，定位时读取新值。 }
          H := TCnMarkDownViewItem(FItems[ItemIndex]).Height;
          if (Host.Left <> ContentLeft) or (Host.Top <> Integer(Y)) or
            (Host.Width <> ContentWidth) or (Host.Height <> H) then
            Host.SetBounds(ContentLeft, Integer(Y), ContentWidth, H);
          { 测量阶段使用过小的格式矩形，最终定位后必须恢复到完整宿主高度。 }
          SetHostFormatRect(Host);
          Host.Visible := True;
          Y := Y + H + FItemSpacing;
        end
        else
          BindHost(Host, -1);
      end;
      Inc(LayoutPass);
    until (not FHeightChanged) or
      (LayoutPass >= MaxLayoutPasses);
  finally
    FHeightChanged := False;
    FInLayout := False;
  end;
end;

procedure TCnMarkDownView.SetScrollOffset(Value: Int64);
begin
  Value := ScrollToPosition(Value);
  if FScrollOffset = Value then
    Exit;
  FScrollOffset := Value;
  UpdateScrollBar;
  LayoutHosts;
  Invalidate;
end;

procedure TCnMarkDownView.ScrollBy(Delta: Int64);
begin
  SetScrollOffset(FScrollOffset + Delta);
end;

procedure TCnMarkDownView.ScrollToBottom;
begin
  SetScrollOffset(MaxScroll);
end;

function TCnMarkDownView.AddMessage(ARole: TCnMarkDownMessageRole): Integer;
begin
  if FFeed = nil then
    raise EInvalidOperation.Create('A Markdown feed is required.');
  Result := FFeed.MessageCount;
  FFeed.AddMessage(ARole);
end;

procedure TCnMarkDownView.AppendToMessage(MessageIndex: Integer;
  const AChunk: TCnMarkDownText);
begin
  if FFeed = nil then
    raise EInvalidOperation.Create('A Markdown feed is required.');
  FFeed.QueueMessage(MessageIndex, AChunk);
end;

procedure TCnMarkDownView.AppendToItem(ItemIndex: Integer;
  const AChunk: TCnMarkDownText);
var
  Item: TCnMarkDownViewItem;
begin
  if FFeed = nil then
    raise EInvalidOperation.Create('A Markdown feed is required.');
  if (ItemIndex < 0) or (ItemIndex >= FItems.Count) then
    raise ERangeError.Create('The Markdown item index is invalid.');
  Item := TCnMarkDownViewItem(FItems[ItemIndex]);
  FFeed.AppendToBlock(Item.MessageIndex, Item.BlockIndex, AChunk);
end;

procedure TCnMarkDownView.AppendToSelectedItem(
  const AChunk: TCnMarkDownText);
var
  ItemIndex: Integer;
begin
  ItemIndex := GetSelectedItemIndex;
  if ItemIndex < 0 then
    raise EInvalidOperation.Create('A visible Markdown item must be selected.');
  AppendToItem(ItemIndex, AChunk);
end;

procedure TCnMarkDownView.FinishMessage(MessageIndex: Integer);
begin
  if FFeed = nil then
    raise EInvalidOperation.Create('A Markdown feed is required.');
  if (MessageIndex < 0) or (MessageIndex >= FFeed.MessageCount) then
    raise ERangeError.Create('The Markdown message index is invalid.');
  if FFeed.Messages[MessageIndex].PendingLength > 0 then
  begin
    FPendingFinishMessage := MessageIndex;
    if FUpdateCount = 0 then
      FFlushTimer.Enabled := True;
  end
  else
    FFeed.FinishMessage(MessageIndex);
end;

procedure TCnMarkDownView.Paint;
begin
  Canvas.Brush.Color := FBackgroundColor;
  Canvas.FillRect(ClientRect);
  inherited Paint;
end;

procedure TCnMarkDownView.Resize;
begin
  inherited Resize;
  CheckPaddingChanged;
  FScrollOffset := ScrollToPosition(FScrollOffset);
  UpdateScrollBar;
  EnsureHosts;
  LayoutHosts;
end;

procedure TCnMarkDownView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_UP: ScrollBy(-LineHeight);
    VK_DOWN: ScrollBy(LineHeight);
    VK_PRIOR: ScrollBy(-ClientHeight);
    VK_NEXT: ScrollBy(ClientHeight);
    VK_HOME: SetScrollOffset(0);
    VK_END: ScrollToBottom;
  end;
end;

procedure TCnMarkDownView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  SetFocus;
end;

procedure TCnMarkDownView.WMVScroll(var Message: TWMVScroll);
var
  SI: TScrollInfo;
  P: Integer;
  TrackMax: Int64;
begin
  SI.cbSize := SizeOf(SI);
  SI.fMask := SIF_ALL;
  GetScrollInfo(Handle, SB_VERT, SI);
  TrackMax := Int64(SI.nMax) - Int64(SI.nPage) + 1;
  if TrackMax < 0 then
    TrackMax := 0;
  P := SI.nPos;
  case Message.ScrollCode of
    SB_TOP: P := SI.nMin;
    SB_BOTTOM: P := Integer(TrackMax);
    SB_LINEUP: Dec(P, LineHeight);
    SB_LINEDOWN: Inc(P, LineHeight);
    SB_PAGEUP: Dec(P, ClientHeight);
    SB_PAGEDOWN: Inc(P, ClientHeight);
    SB_THUMBPOSITION, SB_THUMBTRACK:
      if GetScrollInfo(Handle, SB_VERT, SI) then
        P := SI.nTrackPos
      else
        P := Message.Pos;
  end;
  if P < SI.nMin then
    P := SI.nMin;
  if Int64(P) > TrackMax then
    P := Integer(TrackMax);
  if GetTotalHeight <= MaxInt then
    SetScrollOffset(P)
  else
    SetScrollOffset(ScrollFromPosition(P));
end;

procedure TCnMarkDownView.WMMouseWheel(var Message: TWMMouseWheel);
begin
  ScrollBy(-Int64(Message.WheelDelta) * LineHeight div WHEEL_DELTA);
  Message.Result := 1;
end;

procedure TCnMarkDownView.WMNotify(var Message: TWMNotify);
var
  H: Integer;
  ReqSize: PReqSize;
begin
  if (Message.NMHdr <> nil) and (FMeasureHost <> nil) then
    if (Message.NMHdr^.code = EN_REQUESTRESIZE) and
      FMeasureHost.HandleAllocated and
      (FMeasureHost.Handle = Message.NMHdr^.hwndFrom) then
    begin
      ReqSize := PReqSize(Message.NMHdr);
      H := ReqSize^.rc.bottom - ReqSize^.rc.top + 6;
      FRequestedHeight := H;
    end;
  inherited;
end;

procedure TCnMarkDownRichHost.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if FView <> nil then
    FView.ActivateHost(Self);
end;

procedure TCnMarkDownRichHost.WMMouseWheel(var Message: TWMMouseWheel);
begin
  if FView <> nil then
    FView.ScrollBy(-Int64(Message.WheelDelta) * FView.LineHeight div WHEEL_DELTA)
  else
    inherited;
  Message.Result := 1;
end;

end.
