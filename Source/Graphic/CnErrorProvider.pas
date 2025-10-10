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

unit CnErrorProvider;
{* |<PRE>
================================================================================
* ������ƣ�CnPack ���������
* ��Ԫ���ƣ�CnErrorProvider �ؼ���Ԫ
* ��Ԫ���ߣ�Rain
* ��    ע��Ϊ�û������¼�����ݲ����������������ͼʱ�ṩ���ӻ��ģ�������Ѻõ�
*           ��ʽ�������û�(��Ӱ���û�����¼����Ҳ���صȵ���������û���֪������
*           ����ġ�����˼��ɲμ��û�*�������˼����������)�������ʹ�������
*           ���ں�ʹ���߽����������Եø�רҵ�����Ի���ʹ�û���ýϺ����顣
*           ���κ�Bug�����뵽�ҵ�Blog�������
* ����ƽ̨��PWinXP + Delphi 7.0SP1
* ���ݲ��ԣ�PWin2000/XP/2003 + Delphi 7.0
* �� �� �����õ�Ԫ�в������豾�ػ����ַ���
* �޸ļ�¼��2010-03-07 v0.2
*               ����һ���ͷ��쳣������
*           2008-12-02 19:22 v0.1
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  CnConsts, CnGraphConsts, CnClasses;

type
  TErrorIconAlignment = (iaTopLeft, iaTopRight, iaMiddleLeft, iaMiddleRight,
    iaBottomLeft, iaBottomRight, iaTopCenter, iaBottomCenter, iaUpTopLeft,
    iaDownBottomLeft, iaUpTopRight, iaDownBottomRight);

  TBlinkStyle = (bsAlwaysBlink, bsBlinkIfDifferentError, bsNeverBlink);

  TIconType = (EP_ERROR, EP_ERROR2, EP_INFO, EP_INFO2, EP_WARNING, EP_WARNING2, EP_OK, EP_CUSTOM);

  PErrorStyle = ^TErrorStyle;

  TErrorStyle = packed record
    Hint, Title: string;
    Padding: Integer;
    IconAlignment: TErrorIconAlignment;
    BlinkStyle: TBlinkStyle;
    Icon: TIconType;
  end;

  TSetError = procedure(Sender: TObject; Control: TControl;
    var ES: TErrorStyle; var Result: Boolean) of object;

  TErrorItemClick = procedure(Sender: TObject; ErrorItem: TControl) of object;

  TErrorItemDBClick = procedure(Sender: TObject) of object;

  TCnErrorProviderItem = class;

{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TCnErrorProvider = class(TCnComponent)
  private
    FOwner: TComponent;
    FIconAlignment: TErrorIconAlignment;
    FBlinkStyle: TBlinkStyle;
    FErrorProviderManager: TList;
    FDoubleBuffer: Boolean;
    FClick: TErrorItemClick;
    FDBClick: TErrorItemDBClick;
    FSetError: TSetError;
    procedure SetDoubleBuffer(const Value: Boolean);
    procedure SetClick(const Value: TErrorItemClick);
    procedure SetDBClick(const Value: TErrorItemDBClick);
    function GetControlItems(Index: Integer): TCnErrorProviderItem;
    function GetErrorItemCount: Integer;
    procedure SetSetError(const Value: TSetError);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // ����ָ������Ĵ���ͼʾ
    function SetError(const Control: TControl; ErrorText: string = ''):
      TCnErrorProviderItem; overload;
    function SetError(const Control: TControl; ErrorText: string;
      const IconAlignment: TErrorIconAlignment): TCnErrorProviderItem; overload;
    function SetError(const Control: TControl; ErrorText: string;
      const IconAlignment: TErrorIconAlignment; const BlinkStyle: TBlinkStyle): TCnErrorProviderItem; overload;

    procedure Clear;
    {* ������� ErrorItems}
    procedure Dispose(AOwner: TControl);
    {* �����Ӧ����� ErrorItem}

    property Items[Index: Integer]: TCnErrorProviderItem read GetControlItems; default;
    {* ����������ȡ ErrorItem ����}
    property ErrorItmeCount: Integer read GetErrorItemCount;
    {* ErrorItem ����}
  published
    property DoubleBuffer: Boolean read FDoubleBuffer write SetDoubleBuffer;
    {* �Ƿ�˫���壬����ɾ���������˸}
    property OnClick: TErrorItemClick read FClick write SetClick;
    {* �����¼�}
    property OnDBClick: TErrorItemDBClick read FDBClick write SetDBClick;
    {* ˫���¼�}
    property OnSetError: TSetError read FSetError write SetSetError;
    {* ����SetErrorʱ�������������������һЩͳһ�Ĵ���͵���}
  end;

  TCnErrorProviderItem = class(TGraphicControl)
  private
    FEPOwner: TCnErrorProvider;
    FIcon: TBitmap;
    FControl: TControl;
    FIconAlignment: TErrorIconAlignment;
    FPadding: Integer;
    FHandle: HWND;
    FBlinkStyle: TBlinkStyle;
    FTime: TTime;
    FShow: Boolean;
    FTitle: string;
    FIconType: TIconType;
    FBlinkTime: Integer;
    FBlinkRate: Integer;
    function SpanOfNowAndThen(const ANow, AThen: TDateTime): TDateTime;
    function SecondsBetween(const ANow, AThen: TDateTime): Int64;
    function SecondSpan(const ANow, AThen: TDateTime): Double;
    procedure ChangeControl(const Control: TControl);
    procedure SetIconAlignment(const IconAlignment: TErrorIconAlignment);
    procedure SetControl(const Control: TControl);
    function GetControl: TControl;
    procedure SetTitle(const Value: string);
    function GetErrorIcon: TBitmap;
    procedure SetErrorIcon(const Value: TBitmap);
    procedure SetIconType(const Value: TIconType);
    procedure SetPadding(const Value: Integer);
    procedure SetBlinkTime(const Value: Integer);
    function GetErrorStyle: TErrorStyle;
    procedure SetErrorStyle(const ES: TErrorStyle);
    procedure SetSize;
    procedure SetBlinkRate(const Value: Integer);
  protected
    procedure Paint; override;
    procedure WndProc(var Msg: TMessage); override;
    procedure OnPClick(Sender: TObject); virtual;
    procedure OnPDBClick(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent; const Control: TControl;
      const EP: TCnErrorProvider = nil); reintroduce; virtual;
    destructor Destroy; override;
    // ������������
    procedure SetItem(const IconAlignment: TErrorIconAlignment;
      const Padding: Integer; Control: TControl = nil); overload;
    procedure SetItem(const HintStr: string = '';
      const Title: string = ''); overload;
    procedure SetItem(const IconType: TIconType;
      const BlinkStyle: TBlinkStyle = bsBlinkIfDifferentError); overload;

    // �������
    procedure SetBlinkStyle(const BlinkStyle: TBlinkStyle = bsBlinkIfDifferentError;
      const BlinkRate: Integer = 5);
    property ErrorStyle: TErrorStyle read GetErrorStyle write SetErrorStyle;
    property Canvas;
    property IconAlignment: TErrorIconAlignment read FIconAlignment write SetIconAlignment;
    property BlinkRate: Integer read FBlinkRate write SetBlinkRate;
    property BlinkTime: Integer read FBlinkTime write SetBlinkTime;
    property IconType: TIconType read FIconType write SetIconType;
    property Title: string read FTitle write SetTitle;
    property Padding: Integer read FPadding write SetPadding;
    property Control: TControl read GetControl;
    property ErrorIcon: TBitmap read GetErrorIcon write SetErrorIcon;

    // �¼�
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

{$R CnErrorProvider.res}

const
  CN_EP_TIMERID = 1235;
  CN_EP_CHANGEBLINK = -1;

{ TCnErrorProvider }

procedure TCnErrorProvider.Clear;
var
  I, J: Integer;
  Obj: TCnErrorProviderItem;
begin
  I := FErrorProviderManager.Count;
  if (I <> 0) then
    for J := I - 1 downto 0 do
    begin
      Obj := FErrorProviderManager[J];
      if (Assigned(Obj)) then
      begin
        Obj.Free;
        FErrorProviderManager.Delete(J);
      end;
    end;
end;

constructor TCnErrorProvider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
  if (AOwner is TWinControl) then
    DoubleBuffer := TWinControl(AOwner).DoubleBuffered;
  FErrorProviderManager := TList.Create;
  FIconAlignment := iaMiddleRight;
  FBlinkStyle := bsBlinkIfDifferentError;
end;

destructor TCnErrorProvider.Destroy;
begin
  if (not Assigned(FOwner)) then
    Clear();
  FErrorProviderManager.Free();
  inherited Destroy;
end;

procedure TCnErrorProvider.Dispose(AOwner: TControl);
var
  I, J: Integer;
begin
  I := FErrorProviderManager.Count;
  if (I <> 0) then
  begin
    for J := I - 1 downto 0 do
    begin
      with TCnErrorProviderItem(FErrorProviderManager[J]) do
      begin
        if Control = AOwner then
        begin
          Free;
          FErrorProviderManager.Delete(J);
        end;
      end;
    end;
  end;
end;

function TCnErrorProvider.GetControlItems(Index: Integer): TCnErrorProviderItem;
begin
  Result := nil;
  if (Index < FErrorProviderManager.Count) then
    Result := FErrorProviderManager[Index];
end;

function TCnErrorProvider.GetErrorItemCount: Integer;
begin
  Result := FErrorProviderManager.Count;
end;

procedure TCnErrorProvider.SetClick(const Value: TErrorItemClick);
begin
  FClick := Value;
end;

procedure TCnErrorProvider.SetDBClick(const Value: TErrorItemDBClick);
begin
  FDBClick := Value;
end;

procedure TCnErrorProvider.SetDoubleBuffer(const Value: Boolean);
begin
  FDoubleBuffer := Value;
  if (FOwner is TWinControl) then
    TWinControl(FOwner).DoubleBuffered := Value;
end;

function TCnErrorProvider.SetError(const Control: TControl; ErrorText: string): TCnErrorProviderItem;
var
  Item: TCnErrorProviderItem;
  I: Integer;
  Owner: TWinControl;
  ES: TErrorStyle;
  eResult: Boolean;
begin
  Result := nil;
  eResult := True;
  Owner := nil;

  if (Assigned(Control)) then
  begin
    if (FOwner is TWinControl) then
      Owner := TWinControl(FOwner)
    else if (Control.Parent is TWinControl) then
      Owner := Control.Parent;
    for I := 0 to FErrorProviderManager.Count - 1 do
      if (TCnErrorProviderItem(FErrorProviderManager[I]).Control = Control) then
      begin
        Result := FErrorProviderManager[I];
        ES := Result.ErrorStyle;
        if (Assigned(FSetError)) then
          FSetError(Self, Control, ES, eResult);
        if (eResult) then
          Result.ErrorStyle := ES
        else if not Assigned(FSetError) then
          Result.Hint := ErrorText
        else
          Dispose(Control);
        Exit;
      end;
    ES.IconAlignment := FIconAlignment;
    ES.Hint := ErrorText;
    ES.BlinkStyle := FBlinkStyle;
    ES.Icon := EP_ERROR2;
    ES.Title := '';
    ES.Padding := 5;
    if (Assigned(FSetError)) then
      FSetError(Self, Control, ES, eResult);
    if (eResult) then
    begin
      Item := TCnErrorProviderItem.Create(Owner, Control, Self);
      Item.SetErrorStyle(ES);
      FErrorProviderManager.Add(Item);
      Result := Item;
    end;
  end;
end;

function TCnErrorProvider.SetError(const Control: TControl; ErrorText: string; const IconAlignment: TErrorIconAlignment): TCnErrorProviderItem;
begin
  Self.FIconAlignment := IconAlignment;
  Result := SetError(Control, ErrorText);
end;

function TCnErrorProvider.SetError(const Control: TControl; ErrorText: string; const IconAlignment: TErrorIconAlignment; const BlinkStyle: TBlinkStyle): TCnErrorProviderItem;
begin
  Self.FBlinkStyle := BlinkStyle;
  Result := SetError(Control, ErrorText, IconAlignment);
end;

procedure TCnErrorProvider.SetSetError(const Value: TSetError);
begin
  FSetError := Value;
end;

procedure TCnErrorProvider.GetComponentInfo(var AName, Author, Email, Comment: string);
begin
  AName := SCnErrorProviderName;
  Author := SCnPack_Rain;
  Email := SCnPack_RainEmail;
  Comment := SCnErrorProviderComment;
end;

{ TCnErrorProviderItem }

constructor TCnErrorProviderItem.Create(AOwner: TComponent; const Control: TControl; const EP: TCnErrorProvider);
begin
  inherited Create(AOwner);
  if (AOwner is TWinControl) then
    SetControl(Control);
  Parent.DoubleBuffered := EP.DoubleBuffer;
  FTitle := 'Invalid';
  FBlinkTime := 2;
  FBlinkRate := 5;
  FBlinkStyle := TBlinkStyle(CN_EP_CHANGEBLINK);
  FIcon := TBitmap.Create;
  IconType := EP_ERROR2;
  ShowHint := True;
  FShow := True;
  Canvas.Brush.Style := bsClear;
  Canvas.Font := Font;
  FEPOwner := EP;
  OnClick := OnPClick;
end;

destructor TCnErrorProviderItem.Destroy;
begin
  SetBlinkStyle(bsNeverBlink, 0);
  FreeAndNil(FIcon);
  inherited Destroy;
end;

procedure TCnErrorProviderItem.Paint;
begin
  inherited;
  if (FShow) then
  begin
    Canvas.Draw(0, 0, FIcon);
    if (FTitle <> '') then
      Canvas.TextOut(FIcon.Width + 5, 0, FTitle);
  end;
end;

function TCnErrorProviderItem.GetControl: TControl;
begin
  if (Assigned(FControl)) then
    Result := FControl
  else
    Result := nil;
end;

procedure TCnErrorProviderItem.SetBlinkStyle(const BlinkStyle: TBlinkStyle; const BlinkRate: Integer);
begin
  if (FBlinkStyle <> BlinkStyle) then
  begin
    if (BlinkStyle <> TBlinkStyle(CN_EP_CHANGEBLINK)) then
      FBlinkStyle := BlinkStyle;
    if (FHandle <> 0) then
    begin
      KillTimer(FHandle, CN_EP_TIMERID);
      DeallocateHWnd(FHandle);
      FHandle := 0;
      FShow := True;
      Invalidate();
    end;
    if (FBlinkStyle <> bsNeverBlink) then
      if (FHandle = 0) then
      begin
        FHandle := AllocateHWnd(WndProc);
        SetTimer(FHandle, CN_EP_TIMERID, BlinkRate * 47, nil);
        FTime := Time();
      end;
  end;
end;

procedure TCnErrorProviderItem.SetControl(const Control: TControl);
begin
  if (Assigned(Control)) then
  begin
    FControl := Control;
    ChangeControl(FControl);
  end;
end;

procedure TCnErrorProviderItem.SetIconAlignment(const IconAlignment: TErrorIconAlignment);
begin
  FIconAlignment := IconAlignment;
  if (Assigned(Control)) then
    case FIconAlignment of
      iaTopLeft:
        begin
          Self.Left := FControl.Left - FPadding - Self.Width;
          Self.Top := FControl.Top;
        end;
      iaTopRight:
        begin
          Self.Left := FControl.Left + FControl.Width + FPadding;
          Self.Top := FControl.Top;
        end;
      iaMiddleLeft:
        begin
          Self.Left := FControl.Left - FPadding - Self.Width;
          Self.Top := FControl.Top + FControl.Height - FControl.Height shr 1 - Height shr 1;
        end;
      iaMiddleRight:
        begin
          Self.Left := FControl.Left + FControl.Width + FPadding;
          Self.Top := FControl.Top + FControl.Height - FControl.Height shr 1 - Height shr 1;
        end;
      iaBottomLeft:
        begin
          Self.Left := FControl.Left - FPadding - Self.Width;
          Self.Top := FControl.Top + FControl.Height - Height;
        end;
      iaBottomRight:
        begin
          Self.Left := FControl.Left + FControl.Width + FPadding;
          Self.Top := FControl.Top + FControl.Height - Height;
        end;
      iaTopCenter:
        begin
          Self.Left := FControl.Left + FControl.Width shr 1 - Width shr 1;
          Self.Top := FControl.Top - FPadding - Height;
        end;
      iaBottomCenter:
        begin
          Self.Left := FControl.Left + FControl.Width shr 1 - Width shr 1;
          Self.Top := FControl.Top + FControl.Height + FPadding;
        end;
      iaUpTopLeft:
        begin
          Self.Left := FControl.Left;
          Self.Top := FControl.Top - Self.Height - FPadding;
        end;
      iaDownBottomLeft:
        begin
          Self.Left := FControl.Left;
          Self.Top := FControl.Top + FControl.Height + FPadding;
        end;
      iaUpTopRight:
        begin
          Self.Left := FControl.Left + FControl.Width - Self.Width;
          Self.Top := FControl.Top - Self.Height - FPadding;
        end;
      iaDownBottomRight:
        begin
          Self.Left := FControl.Left + FControl.Width - Self.Width;
          Self.Top := FControl.Top + FControl.Height + FPadding;
        end;
    end;
end;

procedure TCnErrorProviderItem.SetItem(const IconAlignment: TErrorIconAlignment; const Padding: Integer; Control: TControl);
begin
  if (not Assigned(Control)) then
    Control := FControl;
  SetControl(Control);
  FPadding := Padding;
  SetIconAlignment(IconAlignment);
end;

procedure TCnErrorProviderItem.WndProc(var Msg: TMessage);
begin
  inherited;
  if ((Msg.Msg = WM_TIMER) and ((Msg.WParam) = CN_EP_TIMERID)) then
  begin
    if ((FBlinkStyle = bsBlinkIfDifferentError) and (SecondsBetween(Time(), FTime) > FBlinkTime)) then
      SetBlinkStyle(bsNeverBlink, 0)
    else
    begin
      FShow := not FShow;
      Invalidate();
    end;
  end;
end;

procedure TCnErrorProviderItem.SetTitle(const Value: string);
begin
  if (Value <> FTitle) then
  begin
    FTitle := Value;
    SetSize();
    SetIconAlignment(FIconAlignment);
  end;
end;

procedure TCnErrorProviderItem.OnPClick(Sender: TObject);
begin
  if (Assigned(FEPOwner)) and (Assigned(FEPOwner.OnClick)) then
    FEPOwner.OnClick(Self, FControl);
end;

procedure TCnErrorProviderItem.OnPDBClick(Sender: TObject);
begin
  if (Assigned(FEPOwner)) and (Assigned(FEPOwner.OnDBClick)) then
    FEPOwner.OnDBClick(Self);
end;

function TCnErrorProviderItem.GetErrorIcon: TBitmap;
begin
  Result := FIcon;
end;

procedure TCnErrorProviderItem.SetErrorIcon(const Value: TBitmap);
begin
  if (Assigned(Value)) then
  begin
    FIcon.FreeImage();
    FIcon.Assign(Value);
    IconType := EP_CUSTOM;
  end;
end;

procedure TCnErrorProviderItem.SetIconType(const Value: TIconType);
begin
  if (FIconType <> Value) then
  begin
    FIconType := Value;
    if (Assigned(FIcon)) then
    begin
      case Value of
        EP_ERROR:
          FIcon.LoadFromResourceName(HInstance, 'ERRORICON');
        EP_ERROR2:
          FIcon.LoadFromResourceName(HInstance, 'ERRORPROVIDERICON');
        EP_WARNING:
          FIcon.LoadFromResourceName(HInstance, 'WARNINGICON');
        EP_WARNING2:
          FIcon.LoadFromResourceName(HInstance, 'FILEWARNINGICON');
        EP_INFO:
          FIcon.LoadFromResourceName(HInstance, 'INFOICON');
        EP_INFO2:
          FIcon.LoadFromResourceName(HInstance, 'QUESTIONICON');
        EP_OK:
          FIcon.LoadFromResourceName(HInstance, 'OKICON');
      else
        ;
      end;
      FIcon.Transparent := True;
      SetSize;
    end;
  end;
end;

procedure TCnErrorProviderItem.SetPadding(const Value: Integer);
begin
  if (Value <> FPadding) then
  begin
    FPadding := Value;
    SetItem(FIconAlignment, FPadding);
  end;
end;

procedure TCnErrorProviderItem.SetErrorStyle(const ES: TErrorStyle);
begin
  Hint := ES.Hint;
  IconType := ES.Icon;
  Title := ES.Title;
  SetItem(ES.IconAlignment, FPadding);
  SetBlinkStyle(ES.BlinkStyle, FBlinkRate);
  Padding := ES.Padding;
end;

procedure TCnErrorProviderItem.SetBlinkTime(const Value: Integer);
begin
  if (Value <> FBlinkTime) then
    FBlinkTime := Value;
end;

function TCnErrorProviderItem.GetErrorStyle: TErrorStyle;
begin
  with Result do
  begin
    Title := FTitle;
    Hint := Self.Hint;
    Icon := FIconType;
    Padding := FPadding;
    IconAlignment := FIconAlignment;
    BlinkStyle := FBlinkStyle;
  end;
end;

procedure TCnErrorProviderItem.SetSize;
begin
  if (FTitle <> '') then
    Width := FIcon.Width + Canvas.TextWidth(FTitle) + 8
  else
    Width := FIcon.Width;
  Height := FIcon.Height;
  IconAlignment := FIconAlignment;
end;

function TCnErrorProviderItem.SpanOfNowAndThen(const ANow, AThen: TDateTime): TDateTime;
begin
  if ANow < AThen then
    Result := AThen - ANow
  else
    Result := ANow - AThen;
end;

function TCnErrorProviderItem.SecondsBetween(const ANow, AThen: TDateTime): Int64;
begin
  Result := Trunc(SecondSpan(ANow, AThen));
end;

function TCnErrorProviderItem.SecondSpan(const ANow, AThen: TDateTime): Double;
begin
  Result := SecsPerDay * SpanOfNowAndThen(ANow, AThen);
end;

procedure TCnErrorProviderItem.ChangeControl(const Control: TControl);
begin
  if (Assigned(Control.Parent) and (Control.Parent is TWinControl)) then
    Self.Parent := Control.Parent;
end;

procedure TCnErrorProviderItem.SetItem(const HintStr, Title: string);
begin
  Hint := HintStr;
  Self.Title := Title;
  SetItem(FIconAlignment, FPadding, nil);
end;

procedure TCnErrorProviderItem.SetItem(const IconType: TIconType; const BlinkStyle: TBlinkStyle);
begin
  Self.IconType := IconType;
  Self.SetBlinkStyle(BlinkStyle, FBlinkRate);
  SetItem(Hint, FTitle);
end;

procedure TCnErrorProviderItem.SetBlinkRate(const Value: Integer);
begin
  if (FBlinkRate <> Value) then
  begin
    FBlinkRate := Value;
    SetBlinkStyle(TBlinkStyle(CN_EP_CHANGEBLINK), Value);
  end;
end;

end.
