{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2025 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��https://www.cnpack.org                                  }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnSearchCombo;
{* |<PRE>
================================================================================
* ������ƣ�����ؼ���
* ��Ԫ���ƣ������������ؼ���Ԫ��ע����ֹ���Ч���� FPC ����Ч��
* ��Ԫ���ߣ�CnPack ������
* ��    ע����ר�Ұ��ж�������
* ����ƽ̨��Win7 + Delphi 7.0
* ���ݲ��ԣ�Win7 + Delphi 7.0
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2022.10.06 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, Messages, Graphics, Forms, Controls, StdCtrls, Math,
  Menus, {$IFDEF FPC} LCLType, {$ELSE} AppEvnts, {$ENDIF} CnContainers, CnStrings,
  CnEdit;

type
  TCnItemHintEvent = procedure (Sender: TObject; Index: Integer;
    var HintStr: string) of object;

  TCnMatchIndexesPool = class(TCnMathObjectPool)
  {* ƥ���¼�Ĵ洢��}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TList;
    procedure Recycle(Num: TList);
  end;

  TCnFloatListBox = class(TCustomListBox)
  {* �����б��ʵ���࣬��ר�Ұ��ж�������}
  private
    FSelectFontColor: TColor;
    FMatchColor: TColor;
    FSelectBackColor: TColor;
    FShadow: Boolean;
    procedure InitOriginalColors;
    function AdjustHeight(AHeight: Integer): Integer;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNCancelMode(var Message: TMessage); message CM_CANCELMODE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
{$IFNDEF FPC}
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetCount(const Value: Integer);

    procedure SetPos(X, Y: Integer); virtual;
    procedure CloseUp; virtual;
    procedure Popup; virtual;

    property Shadow: Boolean read FShadow write FShadow;
    {* ����ʱ�Ƿ���ʾ��Ӱ���������޸Ĳ���Ч}

    property MatchColor: TColor read FMatchColor write FMatchColor;
    {* ƥ�䲿�ֵ�������ɫ�������Ƿ�ѡ��}
    property SelectBackColor: TColor read FSelectBackColor write FSelectBackColor;
    {* ѡ�в��ֵı���ɫ}
    property SelectFontColor: TColor read FSelectFontColor write FSelectFontColor;
    {* ѡ�в��ֵ�������ɫ}
  end;

  TCnDropDownBox = class(TCnFloatListBox)
  {* �����б��������б�}
  private
    FLastItem: Integer;
    FOnItemHint: TCnItemHintEvent;
    FDisplayItems: TStrings;
    FUpperMatchStr: string;
    FMatchStr: string;
    FMatchMode: TCnMatchMode;
    FItems: TStrings;     // �洢ԭʼ�б�����
    FDisableClickFlag: Boolean;
    FCaseSensitive: Boolean;
    FMatchIndexesPool: TCnMatchIndexesPool;
    FIndent: Integer;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure ListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure SetMatchStr(const Value: string);
    procedure SetIndent(const Value: Integer);
    procedure SetCaseSensitive(const Value: Boolean);
    procedure SetMatchMode(const Value: TCnMatchMode);
  protected
    procedure AdjustListItemHeight;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure ClearDisplayItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Popup; override;
    procedure CloseUp; override;
    procedure Clear; {$IFDEF COMPILER6_UP} override; {$ENDIF}

    procedure UpdateDisplay;

    property DisplayItems: TStrings read FDisplayItems;
    property Items: TStrings read FItems;
    property MatchStr: string read FMatchStr write SetMatchStr;

    property Indent: Integer read FIndent write SetIndent;
    {* �б��������ʱ����������������}
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    {* ƥ��ʱ�Ƿ����ִ�Сд}
    property MatchMode: TCnMatchMode read FMatchMode write SetMatchMode;
    {* ƥ��ģʽ}
    property OnItemHint: TCnItemHintEvent read FOnItemHint write FOnItemHint;
    {* ��Ŀ��ʾ�¼�}
  end;

  TCnSearchComboBox = class(TCnEdit)
  private
    FChangeDown: Boolean;
    FDisableChange: Boolean;
    FOnKillFocus: TNotifyEvent;
    FDropDownList: TCnDropDownBox;
{$IFNDEF FPC}
    FEvents: TApplicationEvents;
{$ENDIF}
    FOnSelect: TNotifyEvent;
    procedure DropDownListDblClick(Sender: TObject);
    procedure DropDownListClick(Sender: TObject);
    procedure UpdateDropPosition;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure ApplicationMessage(var Msg: TMsg; var Handled: Boolean);
    function GetCaseSensitive: Boolean;
    function GetDropDownFont: TFont;
    function GetIndent: Integer;
    function GetMatchColor: TColor;
    function GetMatchMode: TCnMatchMode;
    function GetOnItemHint: TCnItemHintEvent;
    function GetSelectBackColor: TColor;
    function GetSelectFontColor: TColor;
    procedure SetCaseSensitive(const Value: Boolean);
    procedure SetDropDownBackColor(const Value: TColor);
    procedure SetDrowDownFont(const Value: TFont);
    procedure SetIndent(const Value: Integer);
    procedure SetMatchColor(const Value: TColor);
    procedure SetMatchMode(const Value: TCnMatchMode);
    procedure SetOnItemHint(const Value: TCnItemHintEvent);
    procedure SetSelectBackColor(const Value: TColor);
    procedure SetSelectFontColor(const Value: TColor);
    function GetDropDownBackColor: TColor;
    function GetItems: TStrings;
    procedure SetItems(const Value: TStrings);
    function GetItemIndex: Integer;
    procedure SetItemIndex(const Value: Integer);
  protected
    procedure EditButtonClick; override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WndProc(var Message: TMessage); override;
    procedure Change; override;
    procedure DoSelect; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure ShowDropBox;
    procedure CloseUp;
    procedure SetTextWithoutChange(const AText: string);

    property DropDownList: TCnDropDownBox read FDropDownList;
    property ChangeDown: Boolean read FChangeDown write FChangeDown;
    // �Ƿ��������ָı䵼�µ�������Ϊ False ʱ����ǵ�����µ�����

    property Items: TStrings read GetItems write SetItems;
    {* ��������Ŀ��ֱ��ʹ��������� Items}
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    {* ������ѡ�е���Ŀ��ţ�ֱ��ʹ��������� ItemIndex}
    property DropDownFont: TFont read GetDropDownFont write SetDrowDownFont;
    {* ���������壬ֱ��ʹ��������� Font}
    property DropDownBackColor: TColor read GetDropDownBackColor write SetDropDownBackColor;
    {* �����򱳾�ɫ��ֱ��ʹ��������� Color}
    property MatchColor: TColor read GetMatchColor write SetMatchColor;
    {* ƥ�䲿�ֵ�������ɫ�������Ƿ�ѡ�У�ֱ��ʹ���������}
    property SelectBackColor: TColor read GetSelectBackColor write SetSelectBackColor;
    {* ѡ�в��ֵı���ɫ��ֱ��ʹ���������}
    property SelectFontColor: TColor read GetSelectFontColor write SetSelectFontColor;
    {* ѡ�в��ֵ�������ɫ��ֱ��ʹ���������}
    property Indent: Integer read GetIndent write SetIndent;
    {* �б��������ʱ���������������أ�ֱ��ʹ���������}
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    {* ƥ��ʱ�Ƿ����ִ�Сд��ֱ��ʹ���������}
    property MatchMode: TCnMatchMode read GetMatchMode write SetMatchMode;
    {* ƥ��ģʽ��ֱ��ʹ���������}
    property OnItemHint: TCnItemHintEvent read GetOnItemHint write SetOnItemHint;
    {* ��Ŀ��ʾ�¼���ֱ��ʹ���������}

    property OnKillFocus: TNotifyEvent read FOnKillFocus write FOnKillFocus;
    {* ʧ���¼�}
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    {* ѡ����Ŀ���¼�����ʱ Text ������ѡ�е���Ŀ}
  end;

implementation

uses
  CnCommon {$IFDEF DEBUG}, CnDebug {$ENDIF};

const
  CS_DROPSHADOW = $20000;

{ TCnFloatListBox }

function TCnFloatListBox.AdjustHeight(AHeight: Integer): Integer;
var
  BorderSize: Integer;
begin
  BorderSize := Height - ClientHeight;
  Result := Max((AHeight - BorderSize) div ItemHeight, 4) * ItemHeight + BorderSize;
end;

{$IFNDEF FPC}

function TCnFloatListBox.CanResize(var NewWidth,
  NewHeight: Integer): Boolean;
begin
  NewHeight := AdjustHeight(NewHeight);
  Result := True;
end;

{$ENDIF}

procedure TCnFloatListBox.CloseUp;
begin
  Visible := False;
end;

procedure TCnFloatListBox.CNCancelMode(var Message: TMessage);
begin
  CloseUp;
end;

procedure TCnFloatListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
{$IFDEF FPC}
    State := TOwnerDrawState(itemState);
{$ELSE}
    State := TOwnerDrawState(LongRec(itemState).Lo);
{$ENDIF}
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      Canvas.Brush.Color := Color;
      Canvas.Font.Color := Font.Color;
    end;

    if Integer(itemID) >= 0 then
    begin
      if Assigned(OnDrawItem) then
        OnDrawItem(Self, itemID, rcItem, State);
    end
    else
      Canvas.FillRect(rcItem);
    Canvas.Handle := 0;
  end;
end;

procedure TCnFloatListBox.CNMeasureItem(var Message: TWMMeasureItem);
begin
  Message.MeasureItemStruct^.itemHeight := ItemHeight;
end;

constructor TCnFloatListBox.Create(AOwner: TComponent);
begin
  inherited;
  Visible := False;
  Style := lbOwnerDrawFixed;

  FShadow := True;
  InitOriginalColors;

{$IFDEF FPC}
  ItemHeight := 16;
{$ENDIF}

  ShowHint := True;
  Font.Name := 'Tahoma';
  Font.Size := 8;
end;

procedure TCnFloatListBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := (Params.Style or WS_CHILDWINDOW or WS_SIZEBOX or WS_MAXIMIZEBOX
    or LBS_NODATA or LBS_OWNERDRAWFIXED) and not (LBS_SORT or LBS_HASSTRINGS);
  Params.ExStyle := WS_EX_TOOLWINDOW or WS_EX_WINDOWEDGE;

  if FShadow then
    Params.WindowClass.style := CS_DBLCLKS or CS_DROPSHADOW
  else
    Params.WindowClass.style := CS_DBLCLKS;
end;

procedure TCnFloatListBox.CreateWnd;
begin
  inherited;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
  Height := AdjustHeight(Height);
end;

destructor TCnFloatListBox.Destroy;
begin

  inherited;
end;

procedure TCnFloatListBox.InitOriginalColors;
begin
//  FBackColor := clWindow;       // Ĭ�ϵ���δѡ����Ŀ�ı���ɫ������״̬�»��֪����
//  FFontColor := clWindowText;   // Ĭ�ϵ���δѡ����Ŀ��������ɫ������״̬�»��֪����
  FSelectBackColor := clHighlight;      // ѡ����Ŀ�ı���ɫ
  FSelectFontColor := clHighlightText;  // ѡ����Ŀ������ɫ
  FMatchColor := clRed;                 // ƥ��ɫ
end;

procedure TCnFloatListBox.Popup;
begin
  Visible := True;
end;

procedure TCnFloatListBox.SetCount(const Value: Integer);
var
  Error: Integer;
begin
  Error := SendMessage(Handle, LB_SETCOUNT, Value, 0);
  if (Error = LB_ERR) or (Error = LB_ERRSPACE) then
    raise Exception.Create('TCnFloatListBox.SetCount Error: ' + IntToStr(Error));
end;

procedure TCnFloatListBox.SetPos(X, Y: Integer);
begin
  SetWindowPos(Handle, HWND_TOPMOST, X, Y, 0, 0, SWP_NOACTIVATE or SWP_NOSIZE);
end;

{ TCnDropDownBox }

procedure TCnDropDownBox.AdjustListItemHeight;
var
  S, O: Integer;
begin
  try
    // �����ֺű仯��̬���� ItemHeight
    O := Canvas.Font.Size;
    Canvas.Font.Size := Font.Size;
    S := Canvas.TextHeight('Aj');
    Canvas.Font.Size := O;

    if S > 16 then
      S := S + 2
    else
      S := 16; // ��С 16

    if S <> ItemHeight then
      ItemHeight := S;
  except
    ;
  end;
end;

procedure TCnDropDownBox.Clear;
begin
  inherited;
  ClearDisplayItem;
end;

procedure TCnDropDownBox.ClearDisplayItem;
var
  I: Integer;
begin
  for I := 0 to FDisplayItems.Count - 1 do
    if FDisplayItems.Objects[I] <> nil then
      FMatchIndexesPool.Recycle(TList(FDisplayItems.Objects[I]));
  FDisplayItems.Clear;
end;

procedure TCnDropDownBox.CloseUp;
begin
  inherited;

end;

procedure TCnDropDownBox.CMFontChanged(var Message: TMessage);
begin
  if HandleAllocated then
    AdjustListItemHeight;
end;

procedure TCnDropDownBox.CMHintShow(var Message: TMessage);
var
  Index: Integer;
  P: TPoint;
  S: string;
begin
  Message.Result := 1;
  if Assigned(FOnItemHint) and GetCursorPos(P) then
  begin
    P := ScreenToClient(P);
    Index := ItemAtPos(P, True);
    if Index >= 0 then
    begin
      FOnItemHint(Self, Index, S);
      if S <> '' then
      begin
        TCMHintShow(Message).HintInfo^.HintStr := S;
        Message.Result := 0;
      end;
    end;
  end;
end;

constructor TCnDropDownBox.Create(AOwner: TComponent);
const
  csMinDispItems = 6;
  csDefDispItems = 12;
  csDefDispWidth = 300;
begin
  inherited;
  Constraints.MinHeight := ItemHeight * csMinDispItems + 4;
  Height := ItemHeight * csDefDispItems + 8;
  Width := csDefDispWidth;
  Font.Size := 8;
  FLastItem := -1;

  FDisplayItems := TStringList.Create;
  FItems := TStringList.Create;
  FMatchIndexesPool := TCnMatchIndexesPool.Create;
  OnDrawItem := ListDrawItem;
end;

destructor TCnDropDownBox.Destroy;
begin
  FMatchIndexesPool.Free;
  FDisplayItems.Free;
  FItems.Free;
  inherited;
end;

procedure TCnDropDownBox.ListDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  MatchedIndexesRef: TList;
  AText: string;
begin
  if Index >= FDisplayItems.Count then
    Exit;

 // �Ի� ListBox �е� List
  with Control as TCnDropDownBox do
  begin
    Canvas.Font := Font;
    if odSelected in State then
    begin
      Canvas.Brush.Color := FSelectBackColor;
      Canvas.Font.Color  := FSelectFontColor;
    end
    else
    begin
      Canvas.Brush.Color  := Color;
      Canvas.Font.Color := Font.color;
    end;

    Canvas.FillRect(Rect);
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Style := [fsBold];

    AText := FDisplayItems[Index];
    MatchedIndexesRef := nil;
    if FDisplayItems.Objects[Index] <> nil then
      MatchedIndexesRef := FDisplayItems.Objects[Index] as TList;

    DrawMatchText(Canvas, MatchStr, FDisplayItems[Index], Rect.Left +
      FIndent, Rect.Top, FMatchColor, MatchedIndexesRef);
  end;
end;

procedure TCnDropDownBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
  P: TPoint;
begin
  inherited;
  if Shift = [] then
  begin
    P.X := X;
    P.Y := Y;
    Index := ItemAtPos(P, True);
    if Index <> FLastItem then
    begin
      FLastItem := Index;
      Application.CancelHint;
      if Index >= 0 then
      begin
        try
          Selected[Index] := True;
        except
          // �Ͼ� D5 �»�������¡�
          try
            ItemIndex := Index;
          except
            ;
          end;
        end;
        Application.ActivateHint(ClientToScreen(P));
      end;
    end;
  end;
end;

procedure TCnDropDownBox.Popup;
begin
  if not Visible and (Items.Count > 0) then
  begin
    FDisableClickFlag := True;
{$IFDEF DEBUG}
    CnDebugger.LogMsg('Popup. Post a Down to Select.');
{$ENDIF}
    PostMessage(Handle, WM_KEYDOWN, VK_DOWN, 0); // ��������ѡ������
  end;
  inherited;
end;

procedure TCnDropDownBox.SetCaseSensitive(const Value: Boolean);
begin
  if FCaseSensitive <> Value then
  begin
    FCaseSensitive := Value;
    if Visible then
      UpdateDisplay;
  end;
end;

procedure TCnDropDownBox.SetIndent(const Value: Integer);
begin
  if FIndent <> Value then
  begin
    FIndent := Value;
    Invalidate;
  end;
end;

procedure TCnDropDownBox.SetMatchMode(const Value: TCnMatchMode);
begin
  if FMatchMode <> Value then
  begin
    FMatchMode := Value;
    if Visible then
      UpdateDisplay;
  end;
end;

procedure TCnDropDownBox.SetMatchStr(const Value: string);
begin
  if FMatchStr <> Value then
  begin
    FMatchStr := Value;
    FUpperMatchStr := UpperCase(Value);
    if Visible then
      UpdateDisplay;
  end;
end;

procedure TCnDropDownBox.UpdateDisplay;
var
  I: Integer;
  Indexes: TList;
begin
  // ���ݹ����������� FItems ���������������ѡ��ģ����� DisplayItems �ﲢ��ʾ
  ClearDisplayItem;
  if FMatchStr = '' then
  begin
    for I := 0 to FItems.Count - 1 do
      FDisplayItems.Add(FItems[I]);
  end
  else
  begin
    if FCaseSensitive then
    begin
      case FMatchMode of
        mmStart:
          begin
            for I := 0 to FItems.Count - 1 do
              if Pos(FMatchStr, FItems[I]) = 1 then
                FDisplayItems.Add(FItems[I]);
          end;
        mmAnywhere:
          begin
            for I := 0 to FItems.Count - 1 do
              if Pos(FMatchStr, FItems[I]) > 0 then
                FDisplayItems.Add(FItems[I]);
          end;
        mmFuzzy:
          begin
            for I := 0 to FItems.Count - 1 do
            begin
              Indexes := FMatchIndexesPool.Obtain;
              if (FMatchStr = '') or FuzzyMatchStr(FMatchStr, FItems[I], Indexes, True) then
                FDisplayItems.AddObject(FItems[I], Indexes)
              else
                FMatchIndexesPool.Recycle(Indexes);
            end;
          end;
      end;
    end
    else
    begin
      case FMatchMode of
        mmStart:
          begin
            for I := 0 to FItems.Count - 1 do
              if Pos(FUpperMatchStr, UpperCase(FItems[I])) = 1 then
                FDisplayItems.Add(FItems[I]);
          end;
        mmAnywhere:
          begin
            for I := 0 to FItems.Count - 1 do
              if Pos(FUpperMatchStr, UpperCase(FItems[I])) > 0 then
                FDisplayItems.Add(FItems[I]);
          end;
        mmFuzzy:
          begin
            for I := 0 to FItems.Count - 1 do
            begin
              Indexes := FMatchIndexesPool.Obtain;
              if (FMatchStr = '') or FuzzyMatchStr(FMatchStr, FItems[I], Indexes) then
                FDisplayItems.AddObject(FItems[I], Indexes)
              else
                FMatchIndexesPool.Recycle(Indexes);
            end;
          end;
      end;
    end;
  end;

  SetCount(FDisplayItems.Count);
end;

{ TCnMatchIndexesPool }

function TCnMatchIndexesPool.CreateObject: TObject;
begin
  Result := TList.Create;
end;

function TCnMatchIndexesPool.Obtain: TList;
begin
  Result := (inherited Obtain) as TList;
end;

procedure TCnMatchIndexesPool.Recycle(Num: TList);
begin
  inherited Recycle(Num);
end;

{ TCnSearchComboBox }

procedure TCnSearchComboBox.ApplicationMessage(var Msg: TMsg;
  var Handled: Boolean);
begin
  case Msg.message of
    WM_MOUSEWHEEL:  // �����������¼�
      if FDropDownList.Visible then
      begin
        SendMessage(FDropDownList.Handle, WM_MOUSEWHEEL, Msg.wParam, Msg.lParam);
        Handled := True;
        Msg.message := 0;
        Msg.wParam := 0;
        Msg.lParam := 0;
      end;
  end;
end;

procedure TCnSearchComboBox.Change;
var
  OldSel, OldSelLength: Integer;
begin
{$IFDEF DEBUG}
  CnDebugger.LogMsg('Change!.');
{$ENDIF}
  inherited;
  if FDisableChange then
  begin
{$IFDEF DEBUG}
    CnDebugger.LogMsg('Change! Ignore.');
{$ENDIF}
    Exit;
  end;

  OldSel := SelStart;
  OldSelLength := SelLength;

  if Text = '' then
  begin
    FDropDownList.Hide;
    Exit;
  end;

  if not FDropDownList.Visible then
  begin
    FChangeDown := True;
{$IFDEF DEBUG}
    CnDebugger.LogMsg('Change. OnButtonClick for Manually Change Down.');
{$ENDIF}
    if Assigned(OnButtonClick) then // �ֹ�����ǰ�����������¼�
      OnButtonClick(Self);
    FChangeDown := False;
  end;

  FDropDownList.MatchStr := Text;
  FDropDownList.UpdateDisplay;

  if not FDropDownList.Visible then
    ShowDropBox
  else
  begin
    if (FDropDownList.Items.Count > 0) then
    begin
      FDropDownList.FDisableClickFlag := True;
{$IFDEF DEBUG}
      CnDebugger.LogMsg('Change. Post a Down to Select.');
{$ENDIF}
      PostMessage(FDropDownList.Handle, WM_KEYDOWN, VK_DOWN, 0); // ��������ѡ������
    end;
  end;

  SelStart := OldSel;
  SelLength := OldSelLength;
end;

procedure TCnSearchComboBox.CloseUp;
begin
  FDropDownList.CloseUp;
end;

procedure TCnSearchComboBox.CNKeyDown(var Message: TWMKeyDown);
var
  AShortCut: TShortCut;
  ShiftState: TShiftState;
begin
  ShiftState := KeyDataToShiftState(Message.KeyData);
  AShortCut := ShortCut(Message.CharCode, ShiftState);
{$IFNDEF FPC}
  Message.Result := 1;
{$ENDIF}
  if not HandleEditShortCut(Self, AShortCut) then
    inherited;
end;

constructor TCnSearchComboBox.Create(AOwner: TComponent);
begin
  inherited;
  LinkStyle := lsDropDown;
  FDropDownList := TCnDropDownBox.Create(Self);
  FDropDownList.Name := 'CnDropDownList';
  FDropDownList.Parent := Application.MainForm;
  FDropDownList.Width := Width;
  FDropDownList.OnDblClick := DropDownListDblClick;
  FDropDownList.OnClick := DropDownListClick;
  // ע�������ƶ�ѡ��ʱҲ�ᴥ���������ǵ���
  // ����Ҫ�� FDisableClickFlag �����������ں��߷�Χ��

{$IFNDEF FPC}
  FEvents := TApplicationEvents.Create(nil);
  FEvents.OnMessage := ApplicationMessage;
{$ENDIF}
end;

destructor TCnSearchComboBox.Destroy;
begin
{$IFNDEF FPC}
  FEvents.Free;
{$ENDIF}
  inherited;
end;

procedure TCnSearchComboBox.DoSelect;
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;

procedure TCnSearchComboBox.DropDownListClick(Sender: TObject);
begin
{$IFDEF DEBUG}
  CnDebugger.LogMsg('DropDownListClick.');
{$ENDIF}
  if FDropDownList.FDisableClickFlag then
  begin
{$IFDEF DEBUG}
    CnDebugger.LogMsg('DropDownListClick Flag Ignored.');
{$ENDIF}
    FDropDownList.FDisableClickFlag := False;
    Exit;
  end;
  
  if FDropDownList.ItemIndex >= 0 then
  begin
{$IFDEF DEBUG}
    CnDebugger.LogMsg('DropDownListClick to Post a Return.');
{$ENDIF}
    PostMessage(Handle, WM_KEYDOWN, VK_RETURN, 0);
  end;
end;

procedure TCnSearchComboBox.DropDownListDblClick(Sender: TObject);
begin
  PostMessage(Handle, WM_KEYDOWN, VK_RETURN, 0);
end;

procedure TCnSearchComboBox.EditButtonClick;
begin
{$IFDEF DEBUG}
  CnDebugger.LogMsg('EditButtonClick.');
{$ENDIF}
  if FDropDownList.Visible then
    FDropDownList.CloseUp
  else
  begin
    if Text <> '' then
    begin
      Text := '';
      FDropDownList.MatchStr := '';
      DoSelect;
    end;

    ShowDropBox;
  end;
  inherited;
end;

function TCnSearchComboBox.GetCaseSensitive: Boolean;
begin
  Result := FDropDownList.CaseSensitive;
end;

function TCnSearchComboBox.GetDropDownBackColor: TColor;
begin
  Result := FDropDownList.Color;
end;

function TCnSearchComboBox.GetDropDownFont: TFont;
begin
  Result := FDropDownList.Font;
end;

function TCnSearchComboBox.GetIndent: Integer;
begin
  Result := FDropDownList.Indent;
end;

function TCnSearchComboBox.GetItemIndex: Integer;
begin
  if Text = '' then
    Result := FDropDownList.ItemIndex
  else
    Result := FDropDownList.Items.IndexOf(Text);
    // ���˺� DropDownList �� ItemIndex �����ܷ�ӳ��ʵ�������Ҫ�� Text ȥ�ڲ�����
end;

function TCnSearchComboBox.GetItems: TStrings;
begin
  Result := FDropDownList.Items;
end;

function TCnSearchComboBox.GetMatchColor: TColor;
begin
  Result := FDropDownList.MatchColor;
end;

function TCnSearchComboBox.GetMatchMode: TCnMatchMode;
begin
  Result := FDropDownList.MatchMode;
end;

function TCnSearchComboBox.GetOnItemHint: TCnItemHintEvent;
begin
  Result := FDropDownList.OnItemHint;
end;

function TCnSearchComboBox.GetSelectBackColor: TColor;
begin
  Result := FDropDownList.SelectBackColor;
end;

function TCnSearchComboBox.GetSelectFontColor: TColor;
begin
  Result := FDropDownList.SelectFontColor;
end;

procedure TCnSearchComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (Shift = []) then
  begin
    if FDropDownList.Visible then
    begin
      FDropDownList.Hide;
      if Text <> '' then
      begin
        Text := '';
        FDropDownList.MatchStr := '';
        DoSelect;
      end;
    end;
    Key := 0;
  end
  else if (Key = VK_RETURN) and (Shift = []) then
  begin
{$IFDEF DEBUG}
    CnDebugger.LogMsg('KeyDown. Enter Key.');
{$ENDIF}
    // ��������λ�������ָ���ȥ�����ֹ����� Change �¼�
    if FDropDownList.Visible then
    begin
      FDropDownList.Hide;

      FDisableChange := True;
      try
        Text := FDropDownList.DisplayItems[FDropDownList.ItemIndex];
        Change; // ���� inherited �� Change������ FDisableChange ���Ʋ���������� Change����Ϊ����� Change �����������õ�
                                                                                                                           ;
        DoSelect; // ����ѡ���¼�
      finally
        FDisableChange := False;
      end;
    end;
    Key := 0;
  end
  else if Key in [VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT] then
  begin
{$IFDEF DEBUG}
    CnDebugger.LogMsg('KeyDown. Post Navigator Keys to DropDownList.');
{$ENDIF}
    FDropDownList.FDisableClickFlag := True;
    PostMessage(FDropDownList.Handle, WM_KEYDOWN, Key, 0);
    Key := 0;
  end;
  inherited;
end;

procedure TCnSearchComboBox.KeyPress(var Key: Char);
begin
  if Key = #13 then
    Key := #0;
  inherited;
end;

procedure TCnSearchComboBox.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  if (Parent <> nil) and FDropDownList.Visible then
  begin
    FDropDownList.Width := AWidth;
    UpdateDropPosition;
  end;
end;

procedure TCnSearchComboBox.SetCaseSensitive(const Value: Boolean);
begin
  FDropDownList.CaseSensitive := Value;
end;

procedure TCnSearchComboBox.SetDropDownBackColor(const Value: TColor);
begin
  FDropDownList.Color := Value;
end;

procedure TCnSearchComboBox.SetDrowDownFont(const Value: TFont);
begin
  FDropDownList.Font := Value;
end;

procedure TCnSearchComboBox.SetIndent(const Value: Integer);
begin
  FDropDownList.Indent := Value;
end;

procedure TCnSearchComboBox.SetItemIndex(const Value: Integer);
begin
  FDropDownList.ItemIndex := Value;
end;

procedure TCnSearchComboBox.SetItems(const Value: TStrings);
begin
  FDropDownList.Items.Assign(Value);
end;

procedure TCnSearchComboBox.SetMatchColor(const Value: TColor);
begin
  FDropDownList.MatchColor := Value;
end;

procedure TCnSearchComboBox.SetMatchMode(const Value: TCnMatchMode);
begin
  FDropDownList.MatchMode := Value;
end;

procedure TCnSearchComboBox.SetOnItemHint(const Value: TCnItemHintEvent);
begin
  FDropDownList.OnItemHint := Value;
end;

procedure TCnSearchComboBox.SetSelectBackColor(const Value: TColor);
begin
  FDropDownList.SelectBackColor := Value;
end;

procedure TCnSearchComboBox.SetSelectFontColor(const Value: TColor);
begin
  FDropDownList.SelectFontColor := Value;
end;

procedure TCnSearchComboBox.SetTextWithoutChange(const AText: string);
begin
{$IFDEF DEBUG}
  CnDebugger.LogMsg('SetTextWithoutChange ' + AText);
{$ENDIF}
  FDisableChange := True;
  Text := AText;
  FDisableChange := False;
end;

procedure TCnSearchComboBox.ShowDropBox;
begin
  UpdateDropPosition;
  FDropDownList.UpdateDisplay;
  FDropDownList.Popup;
end;

procedure TCnSearchComboBox.UpdateDropPosition;
var
  P: TPoint;
begin
  P.x := Left;
  P.y := Top + Height;
  P := Parent.ClientToScreen(P);
  FDropDownList.SetPos(P.x, P.y);
end;

procedure TCnSearchComboBox.WndProc(var Message: TMessage);
begin
  inherited;
  if Message.Msg = WM_KILLFOCUS then
  begin
    if FDropDownList.Visible then
      FDropDownList.Hide;

    Message.Result := 0;
  end;
end;

end.
