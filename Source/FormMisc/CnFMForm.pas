unit CnFMForm;

interface

uses
  Classes, SysUtils, Controls, Windows, Graphics, StdCtrls, ExtCtrls, Forms,
  Messages, Menus, IniFiles;

type

  TCnFMGlassPanelStyle = (
    gsBlackness, gsDstInvert, gsMergeCopy, gsMergePaint, gsNotSrcCopy,
    gsNotSrcErase, gsPatCopy, gsPatInvert, gsPatPaint, gsSrcAnd,
    gsSrcCopy, gsSrcErase, gsSrcInvert, gsSrcPaint, gsWhiteness);

  TCnFMGlassPanel = class(TCustomControl)
  private
    FColor: TColor;
    FStyle: TCnFMGlassPanelStyle;
    FOnPaint: TNotifyEvent;
    FAttachToNForm: boolean;
    FForm: TForm;

    procedure SetColor(Value: TColor);
    procedure SetStyle(Value: TCnFMGlassPanelStyle);
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure SetAttachToNForm(const Value: boolean);
  protected
    Buffer: TBitmap;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure TitleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderWidth;
    property Color: TColor read FColor write SetColor;
    property Ctl3D;
    property Enabled;
    property Style: TCnFMGlassPanelStyle read FStyle write SetStyle default gsSrcAnd;
    property Visible;
    property Height;
    property Width;
    property Top;
    property Left;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property AttachToNForm: boolean read FAttachToNForm write SetAttachToNForm;
  end;

  TCnFMMenuLabel = class(TCustomLabel)
  private
    FMenu: TPopupMenu;
    FActiveFontColor: TColor;
    FNormalFontColor: TColor;
    FPanel: TCnFMGlassPanel;
  protected
    procedure FMouseEnter(Sender: TObject);
    procedure FMouseLeave(Sender: TObject);
    procedure FClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Menu: TPopupMenu read FMenu write FMenu;
    property Panel: TCnFMGlassPanel read FPanel write FPanel;
    property ActiveFontColor: TColor read FActiveFontColor write FActiveFontColor nodefault;
    property NormalFontColor: TColor read FNormalFontColor write FNormalFontColor nodefault;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EllipsisPosition;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    // property OnClick;
    property OnContextPopup;
    // property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    //property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    //property OnMouseEnter;
    //property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
  end;

  TCnFMMenuImage = class(TImage)
  private
    FMenu: TPopupMenu;
    FPanel: TCnFMGlassPanel;
  protected
    procedure FClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Menu: TPopupMenu read FMenu write FMenu;
    property Panel: TCnFMGlassPanel read FPanel write FPanel;
  end;

  TCnFMTitleImg = class(TImage)
  protected
    procedure FMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCnFMForm = class(TPanel)
  private
    FPTop, FPBottom, FPLeft, FPRight {, FPCenter} : TPanel;
    FIHead, FIHeadLeft, FIHeadRight, FILeft, FIRight, FITile, FITileLeft, FITileRight: TImage;
    // FTransTitle: TCnFMGlassPanel;
    FBtnMin, FBtnMax, FBtnClose: TImage;

    FForm: TForm;
    FOnMaxButtonClick: TNotifyEvent;
    FOnMinButtonClick: TNotifyEvent;
    FOnCloseButtonClick: TNotifyEvent;
    FMinButtonVisible: boolean;
    FCloseButtonVisible: boolean;
    FMaxButtonVisible: boolean;
    FMainForm: TForm;

    FResizable: boolean;
    FGlassPanel: TCnFMGlassPanel;

    FSkined: Boolean;
    FSkinPath: string;
    FDefaultFontActiveColor: TColor;
    FDefaultFontColor: TColor;
    FDefaultColor: TColor;
    procedure SetOnCloseButtonClick(const Value: TNotifyEvent);
    procedure SetOnMaxButtonClick(const Value: TNotifyEvent);
    procedure SetOnMinButtonClick(const Value: TNotifyEvent);
    procedure SetCloseButtonVisible(const Value: boolean);
    procedure SetMaxButtonVisible(const Value: boolean);
    procedure SetMinButtonVisible(const Value: boolean);
    procedure SetResizable(const Value: Boolean);
  protected


    procedure MinMoveEnter(Sender: TObject);
    procedure MinMoveLeave(Sender: TObject);
    procedure MaxMoveEnter(Sender: TObject);
    procedure MaxMoveLeave(Sender: TObject);
    procedure CloseMoveEnter(Sender: TObject);
    procedure CloseMoveLeave(Sender: TObject);

//    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    // resize event
    procedure TopMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TopMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

    procedure BottomMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BottomMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

    procedure LeftMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LeftMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

    procedure RightMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RightMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

    procedure TopLeftMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TopLeftMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

    procedure TopRightMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TopRightMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

    procedure BottomLeftMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BottomLeftMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

    procedure BottomRightMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BottomRightMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
    destructor Destroy; override;
    procedure LoadSkin(AFolder: string);
    procedure SetSideSize(ATop, ABottom, ALeft, Aright: Integer);
    procedure SetSysButtonEnable(E1,E2,E3: Boolean);

    procedure ResetSkin;
    procedure SetButtonPosition;
  published
    property MainForm: TForm read FMainForm write FMainForm;
    property OnCloseButtonClick: TNotifyEvent read FOnCloseButtonClick write SetOnCloseButtonClick;
    property OnMinButtonClick: TNotifyEvent read FOnMinButtonClick write SetOnMinButtonClick;
    property OnMaxButtonClick: TNotifyEvent read FOnMaxButtonClick write SetOnMaxButtonClick;
    property CloseButtonVisible: boolean read FCloseButtonVisible write SetCloseButtonVisible default True;
    property MinButtonVisible: boolean read FMinButtonVisible write SetMinButtonVisible default True;
    property MaxButtonVisible: boolean read FMaxButtonVisible write SetMaxButtonVisible default True;

    property HeadImage: TImage read FIHead write FIHead;
    property HeadLeftCorner: TImage read FIHeadLeft write FIHeadLeft;
    property HeadRightCorner: TImage read FIHeadRight write FIHeadRight;
    property SideLeftImage: TImage read FILeft write FILeft;
    property SideRightImage: TImage read FIRight write FIRight;
    property TileImage: TImage read FITile write FITile;
    property TileLeftCorner: TImage read FITileLeft write FITileLeft;
    property TileRightCorner: TImage read FITileRight write FITileRight;

    property CloseButton: TImage read FBtnClose write FBtnClose;
    property MinButton: TImage read FBtnMin write FBtnMin;
    property MaxButton: TImage read FBtnMax write FBtnMax;

    property TopBar: TPanel read FPTop write FPTop;
    property BottomBar: TPanel read FPBottom write FPBottom;
    property LeftBar: TPanel read FPLeft write FPLeft;
    property RightBar: TPanel read FPRight write FPRight;

    property Resizable: Boolean read FResizable write SetResizable;
    property GlassPanel: TCnFMGlassPanel read FGlassPanel write FGlassPanel;

    property DefaultColor: TColor read FDefaultColor write FDefaultColor default clNone;
    property DefaultFontColor: TColor read FDefaultFontColor write FDefaultFontColor default clNone;
    property DefaultFontActiveColor: TColor read FDefaultFontActiveColor write FDefaultFontActiveColor default clNone;
  end;

implementation

{$R Skin.RES}

const
  mPosLeft = $1;
  mPosRight = $2;
  mPosTop = $3;
  mPosBottom = $4;
  mPosLeftTop = $5;
  mPosLeftBottom = $6;
  mPosRightTop = $7;
  mPosRightBottom = $8;

function RectToIndex(mWidth, mHeight, mSpace, X, Y: Integer; mPos: Integer): Integer;
var
  vPoint: TPoint;
begin
  vPoint := Point(X, Y);
  if (PtInRect(Rect(0, 0, mSpace, mSpace), vPoint) and (mPos = mPosLeftTop)) then
  Result := HTTOPLEFT
  else if (PtInRect(Rect(mWidth - mSpace, mHeight - mSpace, mWidth, mHeight), vPoint) and (mPos = mPosRightBottom)) then
  Result := HTBOTTOMRIGHT
  else if (PtInRect(Rect(mWidth - mSpace, 0, mWidth, mSpace), vPoint) and (mPos = mPosRightTop))  then
  Result := HTTOPRIGHT
  else if (PtInRect(Rect(0, mHeight - mSpace, mSpace, mHeight), vPoint) and (mPos = mPosLeftBottom)) then
  Result := HTBOTTOMLEFT
  else if (mPos = mPosTop) then
  begin
    if PtInRect(Rect(mSpace, 0, mWidth - mSpace, mSpace), vPoint) then
      Result := HTTOP
    else
      Result := $1B;
  end
  else if (PtInRect(Rect(0, mSpace, mSpace, mHeight - mSpace), vPoint) and (mPos = mPosLeft)) then
  Result := HTLEFT
  else if (PtInRect(Rect(mWidth - mSpace, mSpace, mWidth, mHeight - mSpace), vPoint) and (mPos = mPosRight)) then
  Result := HTRIGHT
  else if (PtInRect(Rect(mSpace, mHeight - mSpace, mWidth - mSpace, mHeight), vPoint) and (mPos = mPosBottom)) then
  Result := HTBOTTOM
  else
  Result := -1;
end;
 
function RectToCursor(mWidth, mHeight, mSpace, X, Y: Integer; mPos: Integer): TCursor;
begin  
  case RectToIndex(mWidth, mHeight, mSpace, X, Y, mPos) of
    HTTOPLEFT, HTBOTTOMRIGHT: Result := crSizeNWSE;
    HTTOPRIGHT, HTBOTTOMLEFT: Result := crSizeNESW;
    HTLEFT, HTRIGHT: Result := crSizeWE;
    HTTOP, HTBOTTOM: Result := crSizeNS;
  else Result := crDefault;
  end;
end;

function GlassStyleToInt(gs: TCnFMGlassPanelStyle): LongInt;
begin
  Result := 0;
  case gs of
    gsBlackness  : Result := cmBlackness;
    gsDstInvert  : Result := cmDstInvert;
    gsMergeCopy  : Result := cmMergeCopy;
    gsMergePaint : Result := cmMergePaint;
    gsNotSrcCopy : Result := cmNotSrcCopy;
    gsNotSrcErase: Result := cmNotSrcErase;
    gsPatCopy    : Result := cmPatCopy;
    gsPatInvert  : Result := cmPatInvert;
    gsPatPaint   : Result := cmPatPaint;
    gsSrcAnd     : Result := cmSrcAnd;
    gsSrcCopy    : Result := cmSrcCopy;
    gsSrcErase   : Result := cmSrcErase;
    gsSrcInvert  : Result := cmSrcInvert;
    gsSrcPaint   : Result := cmSrcPaint;
    gsWhiteness  : Result := cmWhiteness;
    else           Assert(True, 'Error parameter in function GlassStyleToInt');
  end;
end;

procedure TCnFMGlassPanel.TitleMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssleft in Shift) then
  begin
    ReleaseCapture;
    if X < 5 then
      FForm.Perform(WM_SYSCOMMAND, $F004, 0)
    else if X > Width - 5 then
      FForm.Perform(WM_SYSCOMMAND, $F005, 0)
    else if Y < 3 then
      FForm.Perform(WM_SYSCOMMAND, $F003, 0)
    else
      FForm.Perform(WM_SYSCOMMAND, $F012, 0);
  end;
end;

constructor TCnFMGlassPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FForm := TForm(AOwner);
  Buffer := TBitmap.Create;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csReplicatable];
  Width := 100;
  Height := 100;
  FStyle := gsSrcAnd;
  ParentCtl3d := False;
  Ctl3D := False;
  ParentColor := False;
  FColor := clWhite;
end;
destructor TCnFMGlassPanel.Destroy;
begin
  Buffer.Free;
  inherited Destroy;
end;
procedure TCnFMGlassPanel.Paint;
var
  R: TRect;
  rop: LongInt;
begin
  R := Rect(0, 0, Width, Height);
  Buffer.Width := Width;
  Buffer.Height := Height;
  Buffer.Canvas.Brush.Style := bsSolid;
  Buffer.Canvas.Brush.Color := FColor;
  Buffer.Canvas.FillRect(Rect(0, 0, Width, Height));
  rop := GlassStyleToInt(FStyle);
  StretchBlt(Buffer.Canvas.Handle, 0, 0, Width, Height,
             Canvas.Handle, 0, 0, Width, Height, rop);
  if Ctl3D then DrawEdge(Buffer.Canvas.Handle, R, BDR_RAISEDINNER, BF_RECT);
  Buffer.Canvas.Pen.Mode := pmCopy;
  Buffer.Canvas.Pen.Style := psSolid;
  Canvas.Draw(0, 0, Buffer);
  if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TCnFMGlassPanel.SetAttachToNForm(const Value: boolean);
var
  cmp: TComponent;
  f: TCnFMForm;
begin
  FAttachToNForm := Value;
  if FAttachToNForm then
  begin
    for cmp in FForm do
    begin
      if cmp is TCnFMForm then
      begin
        f := TCnFMForm(cmp);
        self.Left := 0;
        self.Top := 0;
        self.Height := 22;
        self.Width := f.Width - 100;

        self.BringToFront;
      end;
    end;
    self.OnMouseDown := TitleMouseDown;
  end
  else
    self.OnMouseDown := nil;  
end;

procedure TCnFMGlassPanel.SetColor(Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    RecreateWnd;
  end;
end;

procedure TCnFMGlassPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle + WS_EX_TRANSPARENT;
end;
procedure TCnFMGlassPanel.WMWindowPosChanging(var Message: TWMWindowPosChanging);
begin
  Invalidate;
  inherited;
end;
procedure TCnFMGlassPanel.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 0;
end;
procedure TCnFMGlassPanel.Resize;
begin
  Invalidate;
  inherited;
end;
procedure TCnFMGlassPanel.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  RecreateWnd;
end;
procedure TCnFMGlassPanel.SetStyle(Value: TCnFMGlassPanelStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    RecreateWnd;
  end;
end;

{ TCnFMForm }

procedure TCnFMForm.BottomLeftMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  FForm.Perform(WM_SYSCOMMAND, SC_SIZE + (RectToIndex(TImage(Sender).Width,
    TImage(Sender).Height, 3, X, Y, mPosLeftBottom) - HTLEFT) + 1, 0);
end;

procedure TCnFMForm.BottomLeftMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  TImage(Sender).Cursor := RectToCursor(
    TImage(Sender).Width, TImage(Sender).Height, 3, X, Y, mPosLeftBottom);
end;

procedure TCnFMForm.BottomMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  FForm.Perform(WM_SYSCOMMAND, SC_SIZE + (RectToIndex(TImage(Sender).Width,
    TImage(Sender).Height, 3, X, Y, mPosBottom) - HTLEFT) + 1, 0);
end;

procedure TCnFMForm.BottomMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  TImage(Sender).Cursor := RectToCursor(
    TImage(Sender).Width, TImage(Sender).Height, 3, X, Y, mPosBottom);
end;

procedure TCnFMForm.BottomRightMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  FForm.Perform(WM_SYSCOMMAND, SC_SIZE + (RectToIndex(TImage(Sender).Width,
    TImage(Sender).Height, 3, X, Y, mPosRightBottom) - HTLEFT) + 1, 0);
end;

procedure TCnFMForm.BottomRightMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  TImage(Sender).Cursor := RectToCursor(
    TImage(Sender).Width, TImage(Sender).Height, 3, X, Y, mPosRightBottom);
end;

procedure TCnFMForm.CloseMoveEnter(Sender: TObject);
begin
  if FSkined then
  begin
    if FileExists(FSkinPath + '_Close_High.bmp') then
      FBtnClose.Picture.Bitmap.LoadFromFile(FSkinPath + '_Close_High.bmp');
  end
  else
    FBtnClose.Picture.Bitmap.LoadFromResourceName(HInstance, '_Close_High');
end;

procedure TCnFMForm.CloseMoveLeave(Sender: TObject);
begin
  if FSkined then
  begin
    if FileExists(FSkinPath + '_Close.bmp') then
      FBtnClose.Picture.Bitmap.LoadFromFile(FSkinPath + '_Close.bmp');
  end
  else
    FBtnClose.Picture.Bitmap.LoadFromResourceName(HInstance, '_Close');
end;

constructor TCnFMForm.Create(AOwner: TComponent);
begin
  if not (AOwner is TForm) then
  begin
    MessageBox(TWinControl(AOwner).Handle, 'You must place this component on a form.', 'Error!', MB_OK or MB_ICONINFORMATION);
    FreeAndNil(Self);
    Exit;
  end;
  
  inherited Create(AOwner);

  FSkined := False;
  FDefaultColor := clNone;
  FDefaultFontColor := clNone;
  FDefaultFontActiveColor := clNone;

  FForm := TForm(AOwner);
  FMainForm := FForm;

  FForm.BorderStyle := bsNone;
  FForm.TransparentColor := True;
  FForm.TransparentColorValue := $00FEFEFE;
  FForm.Color := $00FFFBF1;

  BevelOuter := bvNone;
  Align := alClient;

  // Create Panels
  FPTop := TPanel.Create(self);
  FPTop.Parent := self;
  FPTop.Align := alTop;
  FPTop.BevelOuter := bvNone;
  FPTop.Caption := EmptyStr;
  FPTop.Height := 22;

  FPBottom := TPanel.Create(self);
  FPBottom.Parent := self;
  FPBottom.Align := alBottom;
  FPBottom.BevelOuter := bvNone;
  FPBottom.Caption := EmptyStr;
  FPBottom.Height := 5;

  FPLeft := TPanel.Create(self);
  FPLeft.Parent := self;
  FPLeft.Align := alLeft;
  FPLeft.BevelOuter := bvNone;
  FPLeft.Caption := EmptyStr;
  FPLeft.Width := 3;

  FPRight := TPanel.Create(self);
  FPRight.Parent := self;
  FPRight.Align := alRight;
  FPRight.BevelOuter := bvNone;
  FPRight.Caption := EmptyStr;
  FPRight.Width := 3;

  // create images
  FIHeadLeft := TImage.Create(FPTop);
  FIHeadLeft.Parent := FPTop;
  FIHeadLeft.Align := alLeft;
  FIHeadLeft.AutoSize := True;
  FIHeadLeft.Picture.Bitmap.LoadFromResourceName(HInstance, '_Head_Left');

  FIHeadRight := TImage.Create(FPTop);
  FIHeadRight.Parent := FPTop;
  FIHeadRight.Align := alRight;
  FIHeadRight.AutoSize := True;
  FIHeadRight.Picture.Bitmap.LoadFromResourceName(HInstance, '_Head_Right');

  FIHead := TImage.Create(FPTop);
  FIHead.Parent := FPTop;
  FIHead.Align := alClient;
  FIHead.AutoSize := True;
  FIHead.Stretch := True;
  FIHead.Picture.Bitmap.LoadFromResourceName(HInstance, '_Title');

  FITileLeft := TImage.Create(FPBottom);
  FITileLeft.Parent := FPBottom;
  FITileLeft.Align := alLeft;
  FITileLeft.AutoSize := True;
  FITileLeft.Picture.Bitmap.LoadFromResourceName(HInstance, '_Tile_Left');

  FITileRight := TImage.Create(FPBottom);
  FITileRight.Parent := FPBottom;
  FITileRight.Align := alRight;
  FITileRight.AutoSize := True;
  FITileRight.Picture.Bitmap.LoadFromResourceName(HInstance, '_Tile_Right');

  FITile := TImage.Create(FPBottom);
  FITile.Parent := FPBottom;
  FITile.Align := alClient;
  FITile.AutoSize := True;
  FITile.Stretch := True;
  FITile.Picture.Bitmap.LoadFromResourceName(HInstance, '_Tile');

  FILeft := TImage.Create(FPLeft);
  FILeft.Parent := FPLeft;
  FILeft.Align := alClient;
  FILeft.AutoSize := True;
  FILeft.Stretch := True;
  FILeft.Picture.Bitmap.LoadFromResourceName(HInstance, '_Side_Left');

  FIRight := TImage.Create(FPRight);
  FIRight.Parent := FPRight;
  FIRight.Align := alClient;
  FIRight.AutoSize := True;
  FIRight.Stretch := True;
  FIRight.Picture.Bitmap.LoadFromResourceName(HInstance, '_Side_Right');

  FBtnClose := TImage.Create(FPTop);
  FBtnClose.Parent := FPTop;
  FBtnClose.AutoSize := True;
  FBtnClose.Picture.Bitmap.LoadFromResourceName(HInstance, '_Close');
  FBtnClose.Top := 1;
  FBtnClose.Left := self.Width - 50;
  FBtnClose.Anchors := [akTop, akRight];
  FBtnClose.Transparent := True;

  FBtnMax := TImage.Create(FPTop);
  FBtnMax.Parent := FPTop;
  FBtnMax.AutoSize := True;
  FBtnMax.Picture.Bitmap.LoadFromResourceName(HInstance, '_Maximize');
  FBtnMax.Top := 1;
  FBtnMax.Left := self.Width - 76;
  FBtnMax.Anchors := [akTop, akRight];
  FBtnMax.Transparent := True;

  FBtnMin := TImage.Create(FPTop);
  FBtnMin.Parent := FPTop;
  FBtnMin.AutoSize := True;
  FBtnMin.Picture.Bitmap.LoadFromResourceName(HInstance, '_Minimize');
  FBtnMin.Top := 1;
  FBtnMin.Left := self.Width - 103;
  FBtnMin.Anchors := [akTop, akRight];
  FBtnMin.Transparent := True;

  Color := $00FFFBF1;

  FCloseButtonVisible := True;
  FMinButtonVisible := True;
  FMaxButtonVisible := True;
  FIHead.OnMouseDown := TopMouseDown;
  FIHead.OnMouseMove := TopMouseMove;
end;

procedure TCnFMForm.CreateWnd;
begin
  inherited CreateWnd;
  // self
  Caption := EmptyStr;
  SetButtonPosition;
end;

destructor TCnFMForm.Destroy;
begin

  inherited Destroy;
end;

procedure TCnFMForm.LeftMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  FForm.Perform(WM_SYSCOMMAND, SC_SIZE + (RectToIndex(TImage(Sender).Width,
    TImage(Sender).Height, 3, X, Y, mPosLeft) - HTLEFT) + 1, 0);
end;

procedure TCnFMForm.LeftMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  TImage(Sender).Cursor := RectToCursor(
    TImage(Sender).Width, TImage(Sender).Height, 3, X, Y, mPosLeft);
end;

procedure TCnFMForm.LoadSkin(AFolder: string);
var
  fn: string;
  ini: TIniFile;
  iniName: string;
  fc, ffc, ffac: string;
  comp: TComponent;

begin
  // TODO: load skin
  if AFolder[length(AFolder)] <> '\' then
    Afolder :=  AFolder + '\';
  fn := AFolder + '_Head_Left.bmp';
  if FileExists(fn) then
    FIHeadLeft.Picture.Bitmap.LoadFromFile(fn);

  fn := AFolder + '_Head_Right.bmp';
  if FileExists(fn) then
    FIHeadRight.Picture.Bitmap.LoadFromFile(fn);

  fn := AFolder + '_Title.bmp';
  if FileExists(fn) then
    FIHead.Picture.Bitmap.LoadFromFile(fn);

  fn := AFolder + '_Tile_Left.bmp';
  if FileExists(fn) then
    FITileLeft.Picture.Bitmap.LoadFromFile(fn);

  fn := AFolder + '_Tile_Right.bmp';
  if FileExists(fn) then
    FITileRight.Picture.Bitmap.LoadFromFile(fn);
    
  fn := AFolder + '_Tile.bmp';
  if FileExists(fn) then
    FITile.Picture.Bitmap.LoadFromFile(fn);
    
  fn := AFolder + '_Side_Left.bmp';
  if FileExists(fn) then
    FILeft.Picture.Bitmap.LoadFromFile(fn);

  fn := AFolder + '_Side_Right.bmp';
  if FileExists(fn) then
    FIRight.Picture.Bitmap.LoadFromFile(fn);

  fn := AFolder + '_Close.bmp';
  if FileExists(fn) then
    FBtnClose.Picture.Bitmap.LoadFromFile(fn);

  fn := AFolder + '_Maximize.bmp';
  if FileExists(fn) then
    FBtnMax.Picture.Bitmap.LoadFromFile(fn);

  fn := AFolder + '_Minimize.bmp';
  if FileExists(fn) then
    FBtnMin.Picture.Bitmap.LoadFromFile(fn);

  if Assigned(FGlassPanel) then
    FGlassPanel.BringToFront;

  SetButtonPosition;

  iniName := AFolder + 'Skin.ini';
  ini := TIniFile.Create(iniName);

  FPTop.Height := ini.ReadInteger('Skin','Top.Height',3);
  FPBottom.Height := ini.ReadInteger('Skin','Bottom.Height',5);
  FPLeft.Width := ini.ReadInteger('Skin','Left.Width',3);
  FPRight.Width := ini.ReadInteger('Skin','Right.Width',3);

  fc := ini.ReadString('Skin','Form.Color',EmptyStr);
  ffc := ini.ReadString('Skin','Font.Color',EmptyStr);
  ffac := ini.ReadString('Skin', 'Font.ActiveColor', EmptyStr);
  ini.Free;
  if fc <> EmptyStr then
  begin
    FForm.Color := StrToInt('$' + fc);
    self.Color := StrToInt('$' + fc);
  end;
  if ffc <> EmptyStr then
  begin
    for comp in FForm do
    begin
      if comp is TCnFMMenuLabel then
      begin
        TCnFMMenuLabel(comp).Font.Color := StrToInt('$'+ffc);
        TCnFMMenuLabel(comp).NormalFontColor := StrToInt('$'+ffc);
      end;
    end;
  end;
  if ffac <> EmptyStr then
  begin
    for comp in FForm do
    begin
      if comp is TCnFMMenuLabel then
      begin
        TCnFMMenuLabel(comp).ActiveFontColor := StrToInt('$'+ffac);
      end;
    end;
  end;
  FSkined := True;
  FSkinPath := AFolder;
end;

procedure TCnFMForm.MaxMoveEnter(Sender: TObject);
begin
  if FSkined then
  begin
    if FileExists(FSkinPath + '_Maximize_High.bmp') then
      FBtnMax.Picture.Bitmap.LoadFromFile(FSkinPath + '_Maximize_High.bmp')
  end
  else
    FBtnMax.Picture.Bitmap.LoadFromResourceName(HInstance, '_Maximize_High');
end;

procedure TCnFMForm.MaxMoveLeave(Sender: TObject);
begin
  if FSkined then
  begin
    if FileExists(FSkinPath + '_Maximize.bmp') then
      FBtnMax.Picture.Bitmap.LoadFromFile(FSkinPath + '_Maximize.bmp');
  end
  else
    FBtnMax.Picture.Bitmap.LoadFromResourceName(HInstance, '_Maximize');
end;

procedure TCnFMForm.MinMoveEnter(Sender: TObject);
begin
  if FSkined then
  begin
    if FileExists(FSkinPath + '_Minimize_High.bmp') then
      FBtnMin.Picture.Bitmap.LoadFromFile(FSkinPath + '_Minimize_High.bmp');
  end
  else
    FBtnMin.Picture.Bitmap.LoadFromResourceName(HInstance, '_Minimize_High');
end;

procedure TCnFMForm.MinMoveLeave(Sender: TObject);
begin
  if FSkined then
  begin
    if FileExists(FSkinPath + '_Minimize.bmp') then
      FBtnMin.Picture.Bitmap.LoadFromFile(FSkinPath + '_Minimize.bmp');
  end
  else
    FBtnMin.Picture.Bitmap.LoadFromResourceName(HInstance, '_Minimize');
end;

procedure TCnFMForm.ResetSkin;
var
  comp: TComponent;
begin
  // TODO: Reset skin
  FIHeadLeft.Picture.Bitmap.LoadFromResourceName(HInstance, '_Head_Left');
  FIHeadRight.Picture.Bitmap.LoadFromResourceName(HInstance, '_Head_Right');
  FIHead.Picture.Bitmap.LoadFromResourceName(HInstance, '_Title');
  FITileLeft.Picture.Bitmap.LoadFromResourceName(HInstance, '_Tile_Left');
  FITileRight.Picture.Bitmap.LoadFromResourceName(HInstance, '_Tile_Right');
  FITile.Picture.Bitmap.LoadFromResourceName(HInstance, '_Tile');
  FILeft.Picture.Bitmap.LoadFromResourceName(HInstance, '_Side_Left');
  FIRight.Picture.Bitmap.LoadFromResourceName(HInstance, '_Side_Right');
  FBtnClose.Picture.Bitmap.LoadFromResourceName(HInstance, '_Close');
  FBtnMax.Picture.Bitmap.LoadFromResourceName(HInstance, '_Maximize');
  FBtnMin.Picture.Bitmap.LoadFromResourceName(HInstance, '_Minimize');

  FPTop.Height := 22;
  FPBottom.Height := 5;
  FPLeft.Width := 3;
  FPRight.Width := 3;

  FBtnClose.Top := 1;
  FBtnClose.Left := self.Width - 50;
  FBtnMax.Top := 1;
  FBtnMax.Left := self.Width - 76;
  FBtnMin.Top := 1;
  FBtnMin.Left := self.Width - 103;

  if Assigned(FGlassPanel) then
    FGlassPanel.BringToFront;

  SetButtonPosition;
  if FDefaultColor <> clNone then
  begin
    FForm.Color := FDefaultColor;
    self.Color := FDefaultColor;
  end;
  if FDefaultFontColor <> clNone then
  begin
    for comp in FForm do
    begin
      if comp is TCnFMMenuLabel then
      begin
        TCnFMMenuLabel(comp).Font.Color := FDefaultFontColor;
        TCnFMMenuLabel(comp).NormalFontColor := FDefaultFontColor;
      end;
    end;
  end;

  if FDefaultFontActiveColor <> clNone then
  begin
    for comp in FForm do
    begin
      if comp is TCnFMMenuLabel then
        TCnFMMenuLabel(comp).ActiveFontColor := FDefaultFontActiveColor;
    end;
  end;
    
  FSkined := False;
end;

procedure TCnFMForm.RightMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  FForm.Perform(WM_SYSCOMMAND, SC_SIZE + (RectToIndex(TImage(Sender).Width,
    TImage(Sender).Height, 3, X, Y, mPosRight) - HTLEFT) + 1, 0);
end;

procedure TCnFMForm.RightMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  TImage(Sender).Cursor := RectToCursor(
    TImage(Sender).Width, TImage(Sender).Height, 3, X, Y, mPosRight);
end;

procedure TCnFMForm.SetButtonPosition;
begin
  FBtnClose.Left := Self.Width - FBtnClose.Width - 8;
  if FBtnClose.Visible then
    FBtnMax.Left := FBtnClose.Left - FBtnMax.Width
  else
    FBtnMax.Left := self.Width - FBtnMax.Width - 8;
  if FBtnMax.Visible then
    FBtnMin.Left := FBtnMax.Left - FBtnMin.Width
  else
  begin
    if FBtnClose.Visible then
      FBtnMin.Left := FBtnClose.Left - FBtnMin.Width
    else
      FBtnMin.Left := Self.Width - FBtnMin.Width - 8;
  end;
end;

procedure TCnFMForm.SetCloseButtonVisible(const Value: boolean);
begin
  FCloseButtonVisible := Value;
  FBtnClose.Visible := FCloseButtonVisible;
 // SetButtonPosition;
end;

procedure TCnFMForm.SetMaxButtonVisible(const Value: boolean);
begin
  FMaxButtonVisible := Value;
  FBtnMax.Visible := FMaxButtonVisible;
//  SetButtonPosition;
end;

procedure TCnFMForm.SetMinButtonVisible(const Value: boolean);
begin
  FMinButtonVisible := Value;
  FBtnMin.Visible := FMinButtonVisible;
//  SetButtonPosition;
end;

procedure TCnFMForm.SetOnCloseButtonClick(const Value: TNotifyEvent);
begin
  FOnCloseButtonClick := Value;
  if Assigned(FOnCloseButtonClick) then
    FBtnClose.OnClick := FOnCloseButtonClick;
end;

procedure TCnFMForm.SetOnMaxButtonClick(const Value: TNotifyEvent);
begin
  FOnMaxButtonClick := Value;
  if Assigned(FOnMaxButtonClick) then
    FBtnMax.OnClick := FOnMaxButtonClick;
end;

procedure TCnFMForm.SetOnMinButtonClick(const Value: TNotifyEvent);
begin
  FOnMinButtonClick := Value;
  if Assigned(FOnMinButtonClick) then
    FBtnMin.OnClick := FOnMinButtonClick;
end;

procedure TCnFMForm.SetResizable(const Value: Boolean);
begin
  FResizable := Value;
  if FResizable then
  begin
    FILeft.OnMouseDown := LeftMouseDown;
    FILeft.OnMouseMove := LeftMouseMove;
    FIRight.OnMouseDown := RightMouseDown;
    FIRight.OnMouseMove := RightMouseMove;
//    FIHead.OnMouseDown := TopMouseDown;
//    FIHead.OnMouseMove := TopMouseMove;
    FITile.OnMouseDown := BottomMouseDown;
    FITile.OnMouseMove := BottomMouseMove;
    FIHeadLeft.OnMouseDown := TopLeftMouseDown;
    FIHeadLeft.OnMouseMove := TopLeftMouseMove;
    FIHeadRight.OnMouseDown := TopRightMouseDown;
    FIHeadRight.OnMouseMove := TopRightMouseMove;
    FITileLeft.OnMouseDown := BottomLeftMouseDown;
    FITileLeft.OnMouseMove := BottomLeftMouseMove;
    FITileRight.OnMouseDown := BottomRightMouseDown;
    FITileRight.OnMouseMove := BottomRightMouseMove;
  end
  else
  begin
    FILeft.OnMouseDown := nil;
    FILeft.OnMouseMove := nil;
    FIRight.OnMouseDown := nil;
    FIRight.OnMouseMove := nil;
//    FIHead.OnMouseDown := TopMouseDown;
//    FIHead.OnMouseMove := TopMouseMove;
    FITile.OnMouseDown := nil;
    FITile.OnMouseMove := nil;
    FIHeadLeft.OnMouseDown := nil;
    FIHeadLeft.OnMouseMove := nil;
    FIHeadRight.OnMouseDown := nil;
    FIHeadRight.OnMouseMove := nil;
    FITileLeft.OnMouseDown := nil;
    FITileLeft.OnMouseMove := nil;
    FITileRight.OnMouseDown := nil;
    FITileRight.OnMouseMove := nil;
  end;
end;

procedure TCnFMForm.SetSideSize(ATop, ABottom, ALeft, Aright: Integer);
begin
  FPTop.Height := ATop;
  FPBottom.Height := ABottom;
  FPLeft.Width := ALeft;
  FPRight.Width := Aright;
  if Assigned(FGlassPanel) then
    FGlassPanel.BringToFront;
end;

procedure TCnFMForm.SetSysButtonEnable(E1, E2, E3: Boolean);
begin
  FBtnMin.Enabled := E1;
  FBtnMax.Enabled := E2;
  FBtnClose.Enabled := E3;
  if not E1 then
  begin
    FBtnMin.OnMouseEnter := nil;
    FBtnMin.OnMouseLeave := nil;
  end
  else
  begin
    FBtnMin.OnMouseEnter := MinMoveEnter;
    FBtnMin.OnMouseLeave := MinMoveLeave;
  end;

  if not E2 then
  begin
    FBtnMax.OnMouseEnter := nil;
    FBtnMax.OnMouseLeave := nil;
  end
  else
  begin
    FBtnMax.OnMouseEnter := MaxMoveEnter;
    FBtnMax.OnMouseLeave := MaxMoveLeave;
  end;

  if not E3 then
  begin
    FBtnClose.OnMouseEnter := nil;
    FBtnClose.OnMouseLeave := nil;
  end
  else
  begin
    FBtnClose.OnMouseEnter := CloseMoveEnter;
    FBtnClose.OnMouseLeave := CloseMoveLeave;
  end;

end;

procedure TCnFMForm.TopLeftMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  FForm.Perform(WM_SYSCOMMAND, SC_SIZE + (RectToIndex(TImage(Sender).Width,
    TImage(Sender).Height, 3, X, Y, mPosLeftTop) - HTLEFT) + 1, 0);
end;

procedure TCnFMForm.TopLeftMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  TImage(Sender).Cursor := RectToCursor(
    TImage(Sender).Width, TImage(Sender).Height, 3, X, Y, mPosLeftTop);
end;

procedure TCnFMForm.TopMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FResizable then
  begin
    ReleaseCapture;
    // SC_SIZE = $F000
    // HTLEFT = 10;
    FForm.Perform(WM_SYSCOMMAND, SC_SIZE + (RectToIndex(TImage(Sender).Width,
      TImage(Sender).Height, 3, X, Y, mPosTop) - HTLEFT) + 1, 0);
  end
  else
  begin
    ReleaseCapture;
    FForm.Perform(WM_syscommand, $F012, 0);
  end;
end;

procedure TCnFMForm.TopMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  TImage(Sender).Cursor := RectToCursor(
    TImage(Sender).Width, TImage(Sender).Height, 3, X, Y, mPosTop);
end;

procedure TCnFMForm.TopRightMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  FForm.Perform(WM_SYSCOMMAND, SC_SIZE + (RectToIndex(TImage(Sender).Width,
    TImage(Sender).Height, 3, X, Y, mPosRightTop) - HTLEFT) + 1, 0);
end;

procedure TCnFMForm.TopRightMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  TImage(Sender).Cursor := RectToCursor(
    TImage(Sender).Width, TImage(Sender).Height, 3, X, Y, mPosRightTop);
end;

//procedure TCnFMForm.TitleMouseDown(Sender: TObject; Button: TMouseButton;
//  Shift: TShiftState; X, Y: Integer);
//begin
//  if (ssleft in Shift) then
//  begin
//    ReleaseCapture;
//    if X < 5 then
//      FForm.Perform(WM_SYSCOMMAND, $F004, 0)
//    else if X > Width - 5 then
//      FForm.Perform(WM_SYSCOMMAND, $F005, 0)
//    else if Y < 3 then
//      FForm.Perform(WM_SYSCOMMAND, $F003, 0)
//    else
//      FForm.Perform(WM_SYSCOMMAND, $F012, 0);
//  end;
//end;

{ TCnFMMenuLabel }

constructor TCnFMMenuLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Transparent := True;
  OnClick := FClick;
  OnMouseEnter := FMouseEnter;
  OnMouseLeave := FMouseLeave;
end;

procedure TCnFMMenuLabel.FClick(Sender: TObject);
var
  P1,p2: TPoint;
begin
  if (Assigned(Menu)) and (Assigned(Panel)) then
  begin
    P1.x := Self.Left;
    p1.Y := Panel.Height;
    p2 := TForm(Owner).ClientToScreen(p1);
    Menu.Popup(p2.x,p2.y);
  end;
end;

procedure TCnFMMenuLabel.FMouseEnter(Sender: TObject);
begin
  font.Color := FActiveFontColor;
end;

procedure TCnFMMenuLabel.FMouseLeave(Sender: TObject);
begin
  font.Color := FNormalFontColor;
end;

{ TCnFMTitleImg }

constructor TCnFMTitleImg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.Picture.Bitmap.LoadFromResourceName(HInstance, '_FormImg');
  self.AutoSize := True;
  self.Align := alLeft;
  Self.Transparent := True;  
  self.OnMouseDown := FMouseDown;
end;

procedure TCnFMTitleImg.FMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  TForm(Owner).Perform(WM_SYSCOMMAND, $F012, 0);
end;

{ TCnFMMenuImage }

constructor TCnFMMenuImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.Width := 22;
  self.Height := 22;
  Transparent := True;
  OnClick := FClick;
end;

procedure TCnFMMenuImage.FClick(Sender: TObject);
var
  P1,p2: TPoint;
begin
  if (Assigned(Menu)) and (Assigned(Panel)) then
  begin
    P1.x := Self.Left;
    p1.Y := Panel.Height;
    p2 := TForm(Owner).ClientToScreen(p1);
    Menu.Popup(p2.x,p2.y);
  end;
end;

end.
