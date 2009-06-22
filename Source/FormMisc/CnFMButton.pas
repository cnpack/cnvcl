unit CnFMButton;

interface

uses
  windows, messages, classes, sysutils, controls, stdctrls, graphics, Buttons, Forms, Math;

const
  cm_BmpButtonPressed = wm_User + $2020;
  
type
  TCnFMButtonBorder = ( bbNone, bbSingle, bbButton );
  TCnFMBtnSize = ( bszNeither, bszButtonToBitmap, bszStretchToButton, bszTileToButton );
  
  TCnFMButtonBitmaps = class( TPersistent )
  private
    FUp: TBitmap;
    FUpAndFocused: TBitmap;
    FDisabled: TBitmap;
    FDown: TBitmap;
    FStayDown: TBitmap;
    FHot: TBitmap;
    FTransparentColor: TColor;
    FOnChange: TNotifyEvent;

    { Internal Event Handlers }
    procedure BitmapsChanged( Sender: TObject );
  protected
    { Property Access Methods }
    procedure SetUp( Value: TBitmap ); virtual;
    procedure SetUpAndFocused( Value: TBitmap ); virtual;
    procedure SetDisabled( Value: TBitmap ); virtual;
    procedure SetDown( Value: TBitmap ); virtual;
    procedure SetStayDown( Value: TBitmap ); virtual;
    procedure SetHot( Value: TBitmap ); virtual;
    procedure SetTransparentColor( Value: TColor ); virtual;
  public
    constructor Create;
    destructor Destroy; override;
  published
    { Property Declarations }
    property Disabled: TBitmap
      read FDisabled
      write SetDisabled;

    property Down: TBitmap
      read FDown
      write SetDown;

    property StayDown: TBitmap
      read FStayDown
      write SetStayDown;

    property Hot: TBitmap
      read FHot
      write SetHot;

    property TransparentColor: TColor
      read FTransparentColor
      write SetTransparentColor;

    property Up: TBitmap
      read FUp
      write SetUp;

    property UpAndFocused: TBitmap
      read FUpAndFocused
      write SetUpAndFocused;

    property OnChange: TNotifyEvent
      read FOnChange
      write FOnChange;
  end;

 TCnFMButton = class( TCustomControl )
  private
    FGroupIndex: Integer;
    FBitmaps: TCnFMButtonBitmaps;
    FDown: Boolean;
    FCaptionDownOffset: Integer;
    FDragging: Boolean;
    FAllowAllUp: Boolean;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FMargin: Integer;
    FButtonStyle: TButtonStyle;
    FButtonBorder: TCnFMButtonBorder;
    FButtonSize: TCnFMBtnSize;
    FShowFocus: Boolean;
    FShowDownPattern: Boolean;
    FColor: TColor;
    IsFocused: Boolean;
    FModalResult: TModalResult;

    { Internal Event Handlers }
    procedure BitmapChanged( Sender: TObject );

    { Message Handling Methods }
    procedure WMLButtonDblClk( var Msg: TWMLButtonDown ); message wm_LButtonDblClk;
    procedure WMSetFocus( var msg: TWMSetFocus ); message wm_SetFocus;
    procedure WMKillFocus( var msg: TWMKillFocus ); message wm_KillFocus;
    procedure WMSize( var msg: TWMSize ); message wm_Size;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
    procedure CMButtonExtPressed( var Msg: TMessage ); message cm_BmpButtonPressed;
    procedure CMDialogChar( var Msg: TCMDialogChar ); message cm_DialogChar;
    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
    procedure CMTextChanged( var Msg: TMessage ); message cm_TextChanged;
    procedure CMSysColorChange( var Msg: TMessage ); message cm_SysColorChange;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure WMEraseBkgnd( var Msg: TWMEraseBkgnd ); message wm_EraseBkgnd;
  protected
    FState: TButtonState;
    FMouseOverButton: Boolean;

    procedure Loaded; override;
    function GetPalette: HPALETTE; override;
    procedure Paint; override;
    procedure UpdateExclusive;
    procedure CalcLayout( var TextBounds: TRect; var PaintRect: TRect; Bitmap: TBitmap );

    { Event Dispatch Methods }
    procedure ClickButton( DoClick: Boolean );
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure KeyUp( var Key: Word; Shift: TShiftState ); override;
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseMove( Shift: TShiftState; X, Y: Integer ); override;

    { Property Access Methods }
    procedure SetAllowAllUp( Value: Boolean ); virtual;
    procedure SeTCnFMButtonBorder( Value: TCnFMButtonBorder ); virtual;
    procedure SetButtonSize( Value: TCnFMBtnSize ); virtual;
    procedure SetButtonStyle( Value: TButtonStyle ); virtual;
    procedure SetColor( Value: TColor ); virtual;
    procedure SetDown( Value: Boolean ); virtual;
    procedure SetCaptionDownOffset( Value: Integer ); virtual;
    procedure SetGroupIndex( Value: Integer ); virtual;
    procedure SetLayout( Value: TButtonLayout ); virtual;
    procedure SetMargin( Value: Integer ); virtual;
    procedure SetShowDownPattern( Value: Boolean ); virtual;
    procedure SetShowFocus( Value: Boolean ); virtual;
    procedure SetSpacing( Value: Integer ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure Click; override;
  published

    property AllowAllUp: Boolean
      read FAllowAllUp
      write SetAllowAllUp
      default False;

    property CaptionDownOffset: Integer
      read FCaptionDownOffset
      write SetCaptionDownOffset
      default 1;

    property ModalResult: TModalResult
      read FModalResult
      write FModalResult
      default mrNone;

    property GroupIndex: Integer
      read FGroupIndex
      write SetGroupIndex
      default 0;

    { Ensure group index is declared before Down }
    property Down: Boolean
      read FDown
      write SetDown
      default False;

    property Bitmaps: TCnFMButtonBitmaps
      read FBitmaps
      write FBitmaps;

    property Color: TColor
      read FColor
      write SetColor;

    property ButtonBorder: TCnFMButtonBorder
      read FButtonBorder
      write SeTCnFMButtonBorder
      default bbNone;

    property ShowDownPattern: Boolean
      read FShowDownPattern
      write SetShowDownPattern
      default True;

    property ShowFocus: Boolean
      read FShowFocus
      write SetShowFocus
      default True;

    property ButtonSize: TCnFMBtnSize
      read FButtonSize
      write SetButtonSize
      default bszButtonToBitmap;

    property ButtonStyle: TButtonStyle
      read FButtonStyle
      write SetButtonStyle
      default bsAutoDetect;

    property Layout: TButtonLayout
      read FLayout
      write SetLayout
      default blGlyphLeft;

    property Margin: Integer
      read FMargin
      write SetMargin
      default -1;

    property Spacing: Integer
      read FSpacing
      write SetSpacing
      default 4;


    { Inherited Properties & Events }
    property Action;
    property Anchors;
    property Caption;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Height default 30;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property Width default 80;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

 var
  Pattern: TBitmap;
  BmpButtonCount: Integer;

procedure DrawParentImage( Control: TControl; Dest: TCanvas; InvalidateParent: Boolean = False );overload;
procedure DrawParentImage( Control: TControl; DC: HDC; InvalidateParent: Boolean = False ); overload;
procedure CreateBrushPattern;
procedure DrawTransparentBitmap( Canvas: TCanvas; OrigBitmap: TBitmap; Rect: TRect;
                                 Src: TRect; TransparentColor: TColor );

procedure DrawFullTransparentBitmap( Canvas: TCanvas; OrigBitmap: TBitmap; Rect: TRect; Src: TRect;
                                     TransparentColor: TColor );
procedure StretchTransparentBitmap( Canvas:TCanvas; Bitmap: TBitmap; Rect: TRect; TransparentColor: TColor );
procedure TileTransparentBitmap( Canvas: TCanvas; Bitmap: TBitmap; Rect: TRect; TransparentColor: TColor  );

implementation

procedure TileTransparentBitmap( Canvas: TCanvas; Bitmap: TBitmap; Rect: TRect; TransparentColor: TColor  );
var
  X, Y: Integer;
  Dest, Src: TRect;
begin
  Src.Left := 0;
  Src.Top := 0;

  Y := Rect.Top;
  while Y < Rect.Bottom do
  begin
    X := Rect.Left;
    while X < Rect.Right do
    begin
      Dest.Left := X;
      Dest.Top := Y;
      Dest.Right := X + Min( Bitmap.Width, Rect.Right - X );
      Dest.Bottom := Y + Min( Bitmap.Height, Rect.Bottom - Y );

      Src.Right := Dest.Right - Dest.Left;
      Src.Bottom := Dest.Bottom - Dest.Top;

      DrawTransparentBitmap( Canvas, Bitmap, Dest, Src, TransparentColor );
      X := X + Bitmap.Width;
    end;
    Y := Y + Bitmap.Height;
  end;
end;

procedure StretchTransparentBitmap( Canvas:TCanvas; Bitmap: TBitmap; Rect: TRect; TransparentColor: TColor );
var
  Src: TRect;
begin
  { This function requires drawing the entire Bitmap each time... Can't draw just
    subset of the image. }

  Src.Top    := 0;
  Src.Left   := 0;
  Src.Right  := Bitmap.Width;
  Src.Bottom := Bitmap.Height;

  DrawTransparentBitmap( Canvas, Bitmap, Rect, Src, TransparentColor );
end;

procedure DrawFullTransparentBitmap( Canvas: TCanvas; OrigBitmap: TBitmap; Rect: TRect; Src: TRect;
                                     TransparentColor: TColor );
var
  MaskBmp, Bitmap: TBitmap;
  DestW, DestH, SrcW, SrcH: Integer;
begin
  DestW := Rect.Right - Rect.Left;
  DestH := Rect.Bottom - Rect.Top;

  SrcW := Src.Right - Src.Left;
  SrcH := Src.Bottom - Src.Top;

  MaskBmp := TBitmap.Create;
  try
    Bitmap := TBitmap.Create;
    try
      { Make memory Bitmap same size as client rect }

      Bitmap.Width  := DestW;
      Bitmap.Height := DestH;

      StretchBlt( Bitmap.Canvas.Handle, 0, 0, DestW, DestH,
                  OrigBitmap.Canvas.Handle, Src.Left, Src.Top, SrcW, SrcH, SRCCOPY );

      MaskBmp.Monochrome := True;
      MaskBmp.Height := DestH;
      MaskBmp.Width := DestW;

      { Build mask based on transparent color. }

      SetBkColor( Bitmap.Canvas.Handle, ColorToRGB( TransparentColor ) );

      BitBlt( MaskBmp.Canvas.Handle, 0, 0, DestW, DestH, Bitmap.Canvas.Handle, 0, 0, SRCCOPY );

      TransparentStretchBlt( Canvas.Handle, 0, 0, DestW, DestH,
                             Bitmap.Canvas.Handle, 0, 0, SrcW, SrcH,
                             MaskBmp.Canvas.Handle, 0, 0 );

    finally
      Bitmap.free;
    end;
  finally
    MaskBmp.free;
  end;
end; {= DrawFullTransparentBitmap =}

procedure DrawTransparentBitmap( Canvas: TCanvas; OrigBitmap: TBitmap; Rect: TRect;
                                 Src: TRect; TransparentColor: TColor );
var
  MaskBmp, Bitmap: TBitmap;
  DestW, DestH, SrcW, SrcH: Integer;
begin
  DestW := Rect.Right - Rect.Left;
  DestH := Rect.Bottom - Rect.Top;

  SrcW := Src.Right - Src.Left;
  SrcH := Src.Bottom - Src.Top;

  MaskBmp := TBitmap.Create;
  try
    Bitmap := TBitmap.Create;
    try
      { Make memory Bitmap same size as client rect }

      Bitmap.Width  := DestW;
      Bitmap.Height := DestH;

      StretchBlt( Bitmap.Canvas.Handle, 0, 0, DestW, DestH,
                  OrigBitmap.Canvas.Handle, Src.Left, Src.Top, SrcW, SrcH, SRCCOPY );

      MaskBmp.Monochrome := True;
      MaskBmp.Height := DestH;
      MaskBmp.Width := DestW;

      { Build mask based on transparent color. }

      SetBkColor( Bitmap.Canvas.Handle, ColorToRGB( TransparentColor ) );

      BitBlt( MaskBmp.Canvas.Handle, 0, 0, DestW, DestH, Bitmap.Canvas.Handle, 0, 0, SRCCOPY );

      { If transparent color is black, the Bitmap is ready for use.
        Otherwise, put black in the Right place for masking. }
      if TransparentColor <> clBlack then
      begin
        SetBkColor( Bitmap.Canvas.Handle, ColorToRGB( clBlack ) );
        SetTextColor( Bitmap.Canvas.Handle, ColorToRGB( clWhite ) );
        BitBlt( Bitmap.Canvas.Handle, 0, 0, DestW, DestH, MaskBmp.Canvas.Handle, 0, 0, SRCAND );
      end;

      Src := Bounds( 0, 0, DestW, DestH );

      Canvas.CopyMode := cmSrcAnd;
      Canvas.CopyRect( Rect, MaskBmp.Canvas, Src );

      Canvas.CopyMode := cmSrcPaint;
      Canvas.CopyRect( Rect, Bitmap.Canvas, Src );

      { restore orig. Bitmap. }
      if TransparentColor <> clBlack then
      begin
        SetBkColor( Bitmap.Canvas.Handle, ColorToRGB( TransparentColor ) );
        SetTextColor( Bitmap.Canvas.Handle, ColorToRGB( clBlack ) );
        BitBlt( Bitmap.Canvas.Handle, 0, 0, DestW, DestH, MaskBmp.Canvas.Handle, 0, 0, SRCPAINT );
      end;
    finally
      Bitmap.free;
    end;
  finally
    MaskBmp.free;
  end;
end; {= DrawTransparentBitmap =}

procedure CreateBrushPattern;
var
  X: Integer;
  Y: Integer;
begin
  Pattern := TBitmap.Create;
  Pattern.Width := 8;
  Pattern.Height := 8;
  with Pattern.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBtnFace;
    FillRect( Rect( 0, 0, Pattern.Width, Pattern.Height ) );
    for Y := 0 to 7 do
      for X := 0 to 7 do
        if ( Y mod 2 ) = ( X mod 2 ) then
          Pixels[ X, Y ] := clWhite;
  end;
end;

procedure DrawParentImage( Control: TControl; Dest: TCanvas; InvalidateParent: Boolean = False );
begin
  DrawParentImage( Control, Dest.Handle, InvalidateParent );
end;

procedure DrawParentImage( Control: TControl; DC: HDC; InvalidateParent: Boolean = False );
var
  SaveIndex: Integer;
  P: TPoint;
begin
  if Control.Parent = nil then
    Exit;
  SaveIndex := SaveDC( DC );
  GetViewportOrgEx( DC, P );

  SetViewportOrgEx( DC, P.X - Control.Left, P.Y - Control.Top, nil );
  IntersectClipRect( DC, 0, 0, Control.Parent.ClientWidth, Control.Parent.ClientHeight );

  if not ( csDesigning in Control.ComponentState ) then
  begin
    Control.Parent.Perform( wm_EraseBkgnd, DC, 0 );
    Control.Parent.Perform( wm_Paint, DC, 0 );
  end
  else
  begin
    try
      Control.Parent.Perform( wm_EraseBkgnd, DC, 0 );
      Control.Parent.Perform( wm_Paint, DC, 0 );
    except
    end;
  end;

  RestoreDC( DC, SaveIndex );

  if InvalidateParent then
  begin
    if not ( Control.Parent is TCustomControl ) and
       not ( Control.Parent is TCustomForm ) and
       not ( csDesigning in Control.ComponentState ) then
    begin
      Control.Parent.Invalidate;
    end;
  end;
end;

constructor TCnFMButton.Create( AOwner: TComponent );
begin
  inherited;
  Width := 80;
  Height := 30;
  ControlStyle := [ csCaptureMouse, csOpaque, csDoubleClicks ];

  FBitmaps := TCnFMButtonBitmaps.Create;
  FBitmaps.OnChange := BitmapChanged;

  FColor := clBtnFace;
  FButtonBorder := bbNone;
  ParentFont := True;
  FSpacing := 4;
  FMargin := -1;
  FLayout := blGlyphLeft;
  FShowFocus := True;
  FShowDownPattern := True;
  FButtonSize := bszButtonToBitmap;
  TabStop := True;
  IsFocused := False;
  Inc( BmpButtonCount );
  FCaptionDownOffset := 1;
  FMouseOverButton := False;
  {&RCI}
end;


destructor TCnFMButton.Destroy;
begin
  FBitmaps.Free;
  Dec( BmpButtonCount );
  if BmpButtonCount = 0 then
  begin
    Pattern.Free;
    Pattern := nil;
  end;
  inherited;
end;


procedure TCnFMButton.Loaded;
var
  Msg: TWMSize;
begin
  inherited;
  WMSize( Msg );
end;


procedure TCnFMButton.BitmapChanged( Sender: TObject );
var
  Msg: TWMSize;
begin
  {&RV}
  if FButtonSize = bszButtonToBitmap then
    WMSize( Msg );
  Invalidate;
end;

procedure TCnFMButton.WMSize( var msg: TWMSize );
var
  Bevel: Integer;
begin
  if csLoading in ComponentState then
    Exit;

  Bevel := 0;
  if ( FButtonSize <> bszNeither ) and ( FBitmaps.up.handle <> 0 ) then
  begin
    if FButtonSize = bszButtonToBitmap then
    begin
      case FButtonBorder of
        bbNone:
          Bevel := 0;

        bbSingle:
          Bevel := 1;

        bbButton:
          if ( ( FButtonStyle = bsAutoDetect ) and NewStyleControls ) or ( FButtonStyle = bsNew ) then
            Bevel := 2
          else
            Bevel := 3;
      end;
      Width := FBitmaps.Up.Width + ( Bevel shl 1 );
      Height := FBitmaps.Up.Height + ( Bevel shl 1 );
    end;
    Invalidate;
  end;
end; {= TCnFMButton.WMSize =}



procedure TCnFMButton.WMSetFocus( var msg: TWMSetFocus );
begin
  inherited;
  IsFocused := True;
  Invalidate;
end;

procedure TCnFMButton.WMKillFocus( var msg: TWMKillFocus );
begin
  inherited;
  IsFocused := False;
  Invalidate;
end;



procedure TCnFMButton.Paint;
var
  TempBmp, MemImage, DisabledBmp, MonoBmp: TBitmap;
  PaintRect, OrigRect, TextRect, Src, DRect: TRect;
  NewStyle: Boolean;
  Bevel: Integer;
begin
  { Create Memory Bitmap }
  MemImage := TBitmap.Create;
  try
    { Make memory Bitmap same size as client rect }
    MemImage.Height := Height;
    MemImage.Width := Width;

    if ( not Enabled ) and ( not ( csDesigning in ComponentState ) ) then
    begin
      FState := bsDisabled;
      FDragging := False;
    end
    else if FState = bsDisabled then
      FState := bsUp;

    NewStyle := ( ( FButtonStyle = bsAutoDetect ) and NewStyleControls ) or ( FButtonStyle = bsNew );

    if NewStyle then
      Bevel := 1
    else
      Bevel := 2;

    PaintRect := ClientRect;
    MemImage.Canvas.Brush.Color := FColor;

    case FButtonBorder of
      bbNone:
      begin
        { Fill background of MemImage with contents of background }
        DrawParentImage( Self, MemImage.Canvas );
        Sleep( 10 );  { Need to allow short time to get image }

        if csDesigning in ComponentState then
        begin
          with MemImage.Canvas do
          begin
            Pen.Style := psDot;
            Brush.Style := bsClear;
            with ClientRect do
              Rectangle( Left, Top, Right, Bottom );
          end;
        end;
      end;

      bbSingle:
      begin
        MemImage.Canvas.FillRect( PaintRect );
        MemImage.Canvas.Brush.Color := clBlack;
        MemImage.Canvas.FrameRect( PaintRect );
        InflateRect( PaintRect, -1, -1 );
      end;

      bbButton:
      begin
        PaintRect := DrawButtonFace( MemImage.Canvas, ClientRect, Bevel, FButtonStyle,
                                     not NewStyle, FState in [ bsDown, bsExclusive ],
                                     ( IsFocused and FShowFocus ) );
        InflateRect( PaintRect, -1, -1 );
      end;
    end; { case FButtonBorder }

    OrigRect := PaintRect;
    { Is the button going to stay down }
    if FState = bsExclusive then
    begin
      if FShowDownPattern then
      begin
        if Pattern = nil then
          CreateBrushPattern;
        MemImage.Canvas.Brush.Bitmap := Pattern;
      end;

      Dec( PaintRect.Right );
      Dec( PaintRect.Bottom );
      if NewStyle then
      begin
        Dec( PaintRect.Right );
        Dec( PaintRect.Bottom );
      end;
      InflateRect( PaintRect, Bevel, Bevel );
      if FButtonBorder <> bbNone then
        MemImage.Canvas.FillRect( PaintRect );
      InflateRect( PaintRect, -Bevel, -Bevel );

      if NewStyle then
      begin
        Inc( PaintRect.Right );
        Inc( PaintRect.Bottom );
      end;
      Inc( PaintRect.Right );
      Inc( PaintRect.Bottom );
    end;

    { Process any Bitmaps }
    if ( FBitmaps.Up.Handle <> 0 ) or
       ( ( FBitmaps.Disabled.Handle <> 0 ) and ( FState = bsDisabled ) ) then
    begin
      DisabledBmp := TBitmap.Create;
      try
        { Choose/Create the correct Bitmap to display }
        case FState of
          bsUp:
          begin
            if FMouseOverButton and ( FBitmaps.Hot.Handle <> 0 ) then
              TempBmp := FBitmaps.Hot
            else if IsFocused and FShowFocus and ( FBitmaps.UpAndFocused.Handle <> 0 ) then
              TempBmp := FBitmaps.UpAndFocused
            else
              TempBmp := FBitmaps.Up;
          end;

          bsDisabled:
          begin
            if FBitmaps.Disabled.Handle = 0 then
            begin
              { Create a DisabledBmp version of Bitmap }
              DisabledBmp.Width := FBitmaps.Up.Width;
              DisabledBmp.Height := FBitmaps.Up.Height;
              MonoBmp := TBitmap.Create;
              try
                with MonoBmp do
                begin
                  Assign( FBitmaps.Up );
                  Canvas.Brush.Color := clBlack;
                  if Monochrome then
                  begin
                    Canvas.Font.Color := clWhite;
                    Monochrome := False;
                    Canvas.Brush.Color := clWhite;
                  end;
                  Monochrome := True;
                end;
                with DisabledBmp.Canvas do
                begin
                  Brush.Color := clBtnFace;
                  DRect := Bounds( 0, 0, DisabledBmp.width, DisabledBmp.height );
                  FillRect( DRect );
                  Brush.Color := clBlack;
                  Font.Color := clWhite;
                  CopyMode := MergePaint;
                  Draw( DRect.Left + 1, DRect.Top + 1, MonoBmp );
                  CopyMode := SrcAnd;
                  Draw( DRect.Left, DRect.Top, MonoBmp );
                  Brush.Color := clBtnShadow;
                  Font.Color := clBlack;
                  CopyMode := SrcPaint;
                  Draw( DRect.Left, DRect.Top, MonoBmp );
                  CopyMode := SrcCopy;
                end;
              finally
                MonoBmp.Free;
              end;
              TempBmp := DisabledBmp;
            end
            else
              TempBmp := FBitmaps.Disabled;
          end; { bsDisabled }

          bsDown:
          begin
            if FBitmaps.Down.Handle = 0 then
              TempBmp := FBitmaps.Up
            else
              TempBmp := FBitmaps.Down;
          end;

          bsExclusive:
          begin
            if FBitmaps.StayDown.Handle = 0 then
            begin
              if FBitmaps.Down.Handle = 0 then
                TempBmp := FBitmaps.Up
              else
                TempBmp := FBitmaps.Down;
            end
            else
              TempBmp := FBitmaps.StayDown;
          end;

          else
            TempBmp := FBitmaps.Disabled;
        end; { case FState }

        CalcLayout( TextRect, PaintRect, TempBmp );

        { Draw Bitmap }
        case FButtonSize of
          bszNeither:
          begin
            Src := Bounds( 0, 0, TempBmp.Width, TempBmp.Height );
            DrawTransparentBitmap( MemImage.Canvas, TempBmp, PaintRect, Src, FBitmaps.TransparentColor );
          end;

          bszButtonToBitmap:
          begin
            Src := Bounds( 0, 0, TempBmp.Width, TempBmp.Height );
            DrawFullTransparentBitmap( MemImage.Canvas, TempBmp, PaintRect, Src, FBitmaps.TransparentColor );

            (*
            // In future, may want to introduce a Transparent Property to increase performance
            if FTransparent then
              DrawFullTransparentBitmap( MemImage.Canvas, TempBmp, PaintRect, Src, FBitmaps.TransparentColor )
            else
              MemImage.Canvas.Draw( 0, 0, TempBmp );
            *)
          end;

          bszStretchToButton:
            StretchTransparentBitmap( MemImage.Canvas, TempBmp, PaintRect, FBitmaps.TransparentColor );

          bszTileToButton:
            TileTransparentBitmap( MemImage.Canvas, TempBmp, PaintRect, FBitmaps.TransparentColor );

        end; { case FButtonSize }

      finally
        DisabledBmp.Free;
      end;
    end
    else
      CalcLayout( TextRect, PaintRect, nil );

    MemImage.Canvas.Font := Self.Font;

    { Put Caption on Button }
    if Caption <> '' then
    begin
      MemImage.Canvas.Brush.Style := bsClear;
      if FState = bsDisabled then
      begin
        OffsetRect( TextRect, 1, 1 );
        MemImage.Canvas.Font.Color := clWhite;
        DrawText( MemImage.Canvas.Handle, PChar( Caption ), Length( Caption ), TextRect,
                  DT_CENTER or DT_VCENTER or DT_SINGLELINE );
        OffsetRect( TextRect, -1, -1 );
        MemImage.Canvas.Font.Color := clDkGray;
        DrawText( MemImage.Canvas.Handle, PChar( Caption ), Length( Caption ), TextRect,
                  DT_CENTER or DT_VCENTER or DT_SINGLELINE );
      end
      else
      begin
        if ( FState = bsDown ) or ( FState = bsExclusive ) then
          OffsetRect( TextRect, FCaptionDownOffset, FCaptionDownOffset );
        DrawText( MemImage.Canvas.Handle, PChar( Caption ), -1, TextRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE );
        MemImage.Canvas.Font.Color := clWindowText;
      end;

      if IsFocused and FShowFocus then
      begin
        MemImage.Canvas.Brush.Color := clBtnFace;
        InflateRect( TextRect, 2, 2 );
        DrawFocusRect( MemImage.Canvas.Handle, TextRect );
      end;
    end;

    Canvas.CopyMode := cmSrcCopy;
    Canvas.Draw( 0, 0, MemImage );
  finally
    MemImage.Free;
  end;
end; {= TCnFMButton.Paint =}


procedure TCnFMButton.CalcLayout( var TextBounds: TRect; var PaintRect: TRect;
                                   Bitmap: TBitmap );
var
  TextPos: TPoint;
  ClientSize: TPoint;
  BitmapSize: TPoint;
  TextSize: TPoint;
  TotalSize: TPoint;
  Pos: TPoint;
  Spacing: Integer;
  Margin: Integer;
begin
  Canvas.Font := Self.Font;
  { Calculate the item sizes }
  ClientSize := Point( PaintRect.Right - PaintRect.Left, PaintRect.Bottom - PaintRect.Top );

  if ( Bitmap <> nil ) and ( Bitmap.Handle <> 0 ) and ( FButtonSize = bszNeither ) then
    BitmapSize := Point( Bitmap.Width, Bitmap.Height )
  else
    BitmapSize := Point( 0, 0 );

  if Length( Caption ) > 0 then
  begin
    TextBounds := Rect( 0, 0, ClientSize.x, 0 );
    DrawText( Canvas.Handle, PChar( Caption ), -1, TextBounds, DT_CALCRECT );
  end
  else
    TextBounds := Rect( 0, 0, 0, 0 );

  TextSize := Point( TextBounds.Right - TextBounds.Left, TextBounds.Bottom - TextBounds.Top );

  if Layout in [ blGlyphLeft, blGlyphRight ] then
  begin
    Pos.Y := ( ClientSize.Y div 2 ) - ( BitmapSize.Y div 2 );
    TextPos.Y := ( ClientSize.Y div 2 ) - ( TextSize.Y div 2 );
  end
  else
  begin
    Pos.X := ( ClientSize.X div 2 ) - ( BitmapSize.X div 2 );
    TextPos.X := ( ClientSize.X div 2 ) - ( TextSize.X div 2 );
  end;

  Spacing := FSpacing;
  Margin := FMargin;

  { If there is no text or no Bitmap, then Spacing is irrelevant }

  if ( TextSize.X = 0 ) or ( BitmapSize.X = 0 ) then
    Spacing := 0;

  { Adjust Margin and Spacing }

  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point( BitmapSize.X + TextSize.X, BitmapSize.Y + TextSize.Y );
      if Layout in [ blGlyphLeft, blGlyphRight ] then
        Margin := ( ClientSize.X - TotalSize.X ) div 3
      else
        Margin := ( ClientSize.Y - TotalSize.Y ) div 3;

      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point( BitmapSize.X + Spacing + TextSize.X, BitmapSize.Y + Spacing + TextSize.Y );
      if Layout in [ blGlyphLeft, blGlyphRight ] then
        Margin := ( ClientSize.X div 2 ) - ( TotalSize.X div 2 )
      else
        Margin := ( ClientSize.Y div 2 ) - ( TotalSize.Y div 2 );
    end;
  end
  else
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point( ClientSize.X - ( Margin + BitmapSize.X ), ClientSize.Y - ( Margin + BitmapSize.Y ) );
      if Layout in [ blGlyphLeft, blGlyphRight ] then
        Spacing := ( TotalSize.X div 2 ) - ( TextSize.X div 2 )
      else
        Spacing := ( TotalSize.Y div 2 ) - ( TextSize.Y div 2 );
    end;
  end;

  case FLayout of
    blGlyphLeft:
    begin
      Pos.X := Margin;
      TextPos.X := Pos.X + BitmapSize.X + Spacing;
    end;

    blGlyphRight:
    begin
      Pos.X := ClientSize.X - Margin - BitmapSize.X;
      TextPos.X := Pos.X - Spacing - TextSize.X;
    end;

    blGlyphTop:
    begin
      Pos.Y := Margin;
      TextPos.Y := Pos.Y + BitmapSize.Y + Spacing;
    end;

    blGlyphBottom:
    begin
      Pos.Y := ClientSize.Y - Margin - BitmapSize.Y;
      TextPos.Y := Pos.Y - Spacing - TextSize.Y;
    end;
  end; { case Layout }

  OffsetRect( TextBounds, TextPos.X + PaintRect.Left, TextPos.Y + PaintRect.Top );

  if ( BitmapSize.x <> 0 ) and ( BitmapSize.y <> 0 ) then
  begin
    Inc( Pos.X, PaintRect.Left );
    Inc( Pos.Y, PaintRect.Top );
    PaintRect := Rect( Pos.X, Pos.Y, Pos.X + BitmapSize.X, Pos.Y + BitmapSize.Y );
  end;

end;{= TCnFMButton.CalcLayout =}



procedure TCnFMButton.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;
  if Key = vk_Return then
    Click;

  if ( Key = vk_Space ) and Enabled then
  begin
    if not FDown then
    begin
      FState := bsDown;
      Repaint;
    end;
    FDragging := True;
  end;
end;

procedure TCnFMButton.KeyUp( var Key: Word; Shift: TShiftState );
begin
  inherited;

  if FDragging then
  begin
    FDragging := False;
    FState := bsUp;
    ClickButton( True );
  end;
end;


procedure TCnFMButton.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  inherited;

  if Enabled and {not FDown and} not IsFocused and IsWindowVisible( Handle) then
    Windows.SetFocus( Handle );

  if ( Button = mbLeft ) and Enabled and Focused then
  begin
    if not FDown then
    begin
      FState := bsDown;
      Repaint;
    end;
    FDragging := True;
  end;
end;


procedure TCnFMButton.MouseMove( Shift: TShiftState; X, Y: Integer );
var
  NewState: TButtonState;
begin
  inherited;

  if FDragging then
  begin
    if not FDown then
      NewState := bsUp
    else
      NewState := bsExclusive;

    if ( X >= 0 ) and ( X < ClientWidth ) and ( Y >= 0 ) and ( Y <= ClientHeight ) then
      if FDown then
        NewState := bsExclusive
      else
        NewState := bsDown;

    if NewState <> FState then
    begin
      FState := NewState;
      Repaint;
    end;
  end;
end;{= TCnFMButton.MouseMove =}


procedure TCnFMButton.ClickButton( DoClick: Boolean );
begin
  if FGroupIndex = 0 then
    Repaint
  else if DoClick then
    SetDown( not FDown )
  else
  begin
    if FDown then
    begin
      FState := bsExclusive;
    end;
    Repaint;
  end;

  if DoClick then
    Click;
end;

procedure TCnFMButton.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
  DoClick: Boolean;
begin
  inherited;
  if FDragging then
  begin
    FDragging := False;
    DoClick := ( X >= 0 ) and ( X < ClientWidth ) and ( Y >= 0 ) and ( Y <= ClientHeight );
    UpdateExclusive;

    ClickButton( DoClick );
  end;
end;{= TCnFMButton.MouseUp =}


procedure TCnFMButton.Click;
var
  Form: TCustomForm;
begin
  {&RV}
  Form := GetParentForm( Self );
  if Form <> nil then
    Form.ModalResult := ModalResult;
  inherited;
end;


function TCnFMButton.GetPalette: HPALETTE;
begin
  Result := FBitmaps.Up.Palette;
end;


procedure TCnFMButton.UpdateExclusive;
var
  Msg: TMessage;
begin
  if ( FGroupIndex <> 0 ) and ( Parent <> nil ) then
  begin
    Msg.Msg := cm_BmpButtonPressed;
    Msg.WParam := FGroupIndex;
    Msg.LParam := Longint( Self );
    Msg.Result := 0;
    Parent.Broadcast( Msg );
  end
  else
    FState := bsUp;
end;


procedure TCnFMButton.SetCaptionDownOffset( Value: Integer );
begin
  if FCaptionDownOffset <> Value then
  begin
    FCaptionDownOffset := Value;
    Invalidate;
  end;
end;


procedure TCnFMButton.SetDown( Value: Boolean );
begin
  if FGroupIndex = 0 then
    Value := False;

  if Value <> FDown then
  begin
    if FDown and ( not FAllowAllUp ) then
      Exit;

    FDown := Value;
    if Value then
    begin
      FState := bsExclusive;
    end
    else
    begin
      FState := bsUp;
    end;

    Invalidate;

    if Value then
      UpdateExclusive;
  end;
end; {= TCnFMButton.SetDown =}


procedure TCnFMButton.SetGroupIndex( Value: Integer );
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;


procedure TCnFMButton.SetLayout( Value: TButtonLayout );
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TCnFMButton.SetMargin( Value: Integer );
begin
  if ( FMargin <> Value ) and ( Value >= -1 ) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TCnFMButton.SetSpacing( Value: Integer );
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TCnFMButton.SetAllowAllUp( Value: Boolean );
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;


procedure TCnFMButton.SetShowDownPattern( Value: Boolean );
begin
  if FShowDownPattern <> Value then
  begin
    FShowDownPattern := Value;
    Invalidate;
  end;
end;


procedure TCnFMButton.SetShowFocus( Value: Boolean );
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    Invalidate;
  end;
end;


procedure TCnFMButton.WMLButtonDblClk( var Msg: TWMLButtonDown );
begin
  inherited;
  if FDown then
    DblClick;
end;


procedure TCnFMButton.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  Invalidate;
end;


procedure TCnFMButton.CMButtonExtPressed( var Msg: TMessage );
var
  Sender: TCnFMButton;
begin
  if Msg.WParam = FGroupIndex then
  begin
    Sender := TCnFMButton( Msg.LParam );
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := bsUp;
        Invalidate;
      end;

      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;


procedure TCnFMButton.CMDialogChar( var Msg: TCMDialogChar );
begin
  with Msg do
  begin
    if IsAccel( CharCode, Caption ) and Enabled then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
  end;
end;


procedure TCnFMButton.CMFontChanged( var Msg: TMessage );
begin
  Invalidate;
end;

procedure TCnFMButton.CMTextChanged( var Msg: TMessage );
begin
  Invalidate;
end;

procedure TCnFMButton.CMSysColorChange( var Msg: TMessage );
begin
  Invalidate;
end;

procedure TCnFMButton.SetButtonStyle( Value: TButtonStyle );
var
  Msg: TWMSize;
begin
  if FButtonStyle <> Value then
  begin
    FButtonStyle := Value;
    WMSize( Msg );
    Invalidate;
  end;
end;

procedure TCnFMButton.SetColor( Value: TColor );
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Invalidate;
  end;
end;

procedure TCnFMButton.SeTCnFMButtonBorder( Value: TCnFMButtonBorder );
var
  Msg: TWMSize;
begin
  if FButtonBorder <> Value then
  begin
    FButtonBorder := Value;
    WMSize( Msg );
    Invalidate;
  end;
end;

procedure TCnFMButton.SetButtonSize( Value: TCnFMBtnSize );
var
  Msg: TWMSize;
begin
  if FButtonSize <> Value then
  begin
    FButtonSize := Value;
    WMSize( Msg );
    Invalidate;
  end;
end;


procedure TCnFMButton.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  FMouseOverButton := True;

  inherited;

  if FBitmaps.Hot.Handle <> 0 then
    Invalidate;
end;


procedure TCnFMButton.CMMouseLeave( var Msg: TMessage );
begin
  FMouseOverButton := False;

  inherited;
  if FBitmaps.Hot.Handle <> 0 then
    Invalidate;
end;


procedure TCnFMButton.WMEraseBkgnd( var Msg: TWMEraseBkgnd );
begin
  // Do not call inherited -- prevents TWinControl.WMEraseBkgnd from
  // erasing background. Set Msg.Result to 1 to indicate background is painted
  // by the control.
  Msg.Result := 1;
end;


constructor TCnFMButtonBitmaps.Create;
begin
  FUp := TBitmap.Create;
  FUp.OnChange := BitmapsChanged;
  FUpAndFocused := TBitmap.Create;
  FUpAndFocused.OnChange := BitmapsChanged;
  FDisabled := TBitmap.Create;
  FDisabled.OnChange := BitmapsChanged;
  FDown := TBitmap.Create;
  FDown.OnChange := BitmapsChanged;
  FStayDown := TBitmap.Create;
  FStayDown.OnChange := BitmapsChanged;
  FHot := TBitmap.Create;
  FHot.OnChange := BitmapsChanged;
  FTransparentColor := clOlive;
end;


destructor TCnFMButtonBitmaps.Destroy;
begin
  FUp.Free;
  FUpAndFocused.Free;
  FDisabled.Free;
  FDown.Free;
  FStayDown.Free;
  FHot.Free;
  inherited;
end;


procedure TCnFMButtonBitmaps.SetUp( Value: TBitmap );
begin
  FUp.Assign( Value );
end;


procedure TCnFMButtonBitmaps.SetUpAndFocused( Value: TBitmap );
begin
  FUpAndFocused.Assign( Value );
end;


procedure TCnFMButtonBitmaps.SetDisabled( Value: TBitmap );
begin
  FDisabled.Assign( Value );
end;


procedure TCnFMButtonBitmaps.SetDown( Value: TBitmap );
begin
  FDown.Assign( Value );
end;


procedure TCnFMButtonBitmaps.SetStayDown( Value: TBitmap );
begin
  FStayDown.Assign( Value );
end;


procedure TCnFMButtonBitmaps.SetHot( Value: TBitmap );
begin
  FHot.Assign( Value );
end;


procedure TCnFMButtonBitmaps.BitmapsChanged( Sender: TObject );
begin
  if Assigned( FOnChange ) then
    FOnChange( Self );
end;


procedure TCnFMButtonBitmaps.SetTransparentColor( Value: TColor );
begin
  if FTransparentColor <> Value then
  begin
    FTransparentColor := Value;
    BitmapsChanged( Self );
  end;
end;


end.
