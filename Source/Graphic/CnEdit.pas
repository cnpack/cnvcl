{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2025 CnPack 开发组                       }
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

unit CnEdit;
{* |<PRE>
================================================================================
* 软件名称：界面控件包
* 单元名称：CnEdit 控件单元
* 单元作者：盛小青  chbsxq@163.com   QQ:822154
*           jAmEs_
* 备    注：-使 CnEdit 带有一个按钮，按钮拥有单击事件。
*           -LinkStyle 属性可以设置为lsNone, lsEllipsis, lsDropDown
*
*           -CnEdit 的 Text 类型可以为整形、浮点型、普通文字型、标识符型
*           -TextType 属性可以设置为 NormalText, IntegerText, FloatText
*           -TextType 设置为 IntegerText 时，CnEdit 只接受数字键与 Backspace 键，其它按键无效
*            同样，设为 FloatText 只比 IntegerText 多能接受 '.'，但如果 CnEdit.Text 已包含'.'则不能再输入.
*            -对负号'-'，只能在开头输入.
*           -FloatText 类型时，输入'0.'或者'.0'这样的情况会自动修正为'0.0'
*           -CnEdit 失去焦点时会检查 Text，如果不符合 TextType 的设置，则清空或置 0
*            这样避免了粘贴进来不合法字符
*
*           -具备回车键替换成 Tab 键的功能，只要设置属性 EnterAsTab 为 True，
*            则在 CnEdit 件中按回车键将自动跳至下一控件。
* 开发平台：PWinXP + Delphi 6.0
* 兼容测试：PWin9X/2000/XP + Delphi 6.0
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2025.04.29 V1.6
*               增加部分主题颜色的支持
*           2025.03.29 V1.5
*               增加一个 PaddingWidth 属性控制文本横向绘制的缩进像素数，默认 0
*           2022.03.26 V1.4
*               调整大尺寸时的按钮绘制细节
*           2009.07.04 V1.3
*               修正 tArightJustify 时绘制不正确的问题，感谢 jAmEs_
*           2008.06.05 V1.2
*               处理粘贴时的限制内容
*           2007.08.02 V1.1
*               jAmEs_ 加入 Value 属性，加入标识符型、文字过滤和负数控制功能
*           2004.04.24 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, {$IFDEF FPC} LCLType, {$ENDIF} Controls,
  StdCtrls, Forms, Graphics, Clipbrd {$IFDEF SUPPORT_THEME}, Vcl.Themes {$ENDIF};

type
  TLinkStyle = (lsNone, lsEllipsis, lsDropDown); // 是否出现按钮以及按钮类型
  TTextType = (NormalText, IntegerText, FloatText, IdentText); // 文本类型
  //           普通文本、  整数、       小数、     标识符

  TCnPaintPaddingEvent = procedure(Sender: TObject; Canvas: TCanvas; PaddingRect: TRect) of object;

{$IFNDEF FPC}
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
{$ENDIF}
  TCnEdit = class(TEdit)
  private
    FButtonWidth: Integer;
    FPaddingWidth: Integer;
    FCanvas: TControlCanvas;
    FLinkStyle: TLinkStyle;
    FAlignment: TAlignment;
    FPressed: Boolean;
    FTracking: Boolean;
    FOnButtonClick: TNotifyEvent;
    FTextType: TTextType; // 文本类型、整形、浮点、文字
    FEnterAsTab: Boolean; // 回车做为 Tab
    FAcceptNegative: Boolean;
    FAcceptCharList: string;
    FButtonCursor: TCursor;
    FOnPaintPadding: TCnPaintPaddingEvent;
    procedure SetLinkStyle(Value: TLinkStyle); // 设置是否显示按钮
    procedure TrackButton(X, Y: Integer); // 跟踪鼠标按下按钮移开又回来的情况。按下触发
    procedure StopTracking; //同上。up 触发
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    function GetTextMargins: TPoint; // Text 编辑区边上的空白
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR; // 设置鼠标在按钮上的箭头
    function GetValue: Variant;
    procedure SetButtonCursor(const Value: TCursor);
    procedure SetPaddingWidth(const Value: Integer);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure EditButtonClick; virtual; // 单击事件
    procedure BoundsChanged;
    procedure CreateParams(var Params: TCreateParams); override; // 这个非常有用
    procedure DoEnter; override; // 获取焦点时选择全部文字
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override; // 快捷键
    procedure KeyPress(var Key: Char); override; // 必须屏蔽回车键，因为是多行方式，但不允许换行
    // 在 MouseUp 触发按钮事件，其它事件画按钮状态
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure DoPaintPadding(Canvas: TCanvas; PaddingRect: TRect); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Value: Variant read GetValue;
  published
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    {* 右侧按钮点击事件}
    property LinkStyle: TLinkStyle read FLinkStyle write SetLinkStyle default lsNone;
    {* 按钮类型}
    property ButtonCursor: TCursor read FButtonCursor write SetButtonCursor default crDefault;
    {* 按钮上的光标}
    property ButtonWidth: Integer read FButtonWidth;
    {* 按钮宽度，只读}
    property PaddingWidth: Integer read FPaddingWidth write SetPaddingWidth;
    {* 文本内的对齐偏移量，默认 0}
    property Alignment: TAlignment read FAlignment write FAlignment default TaLeftJustify;
    {* 对齐方式}
    property TextType: TTextType read FTextType write FTextType default NormalText;
    {* 文本类型}
    property EnterAsTab: Boolean read FEnterAsTab write FEnterAsTab default False;
    {* 回车键是否转换为 Tab 键}
    property AcceptNegative: Boolean read FAcceptNegative write FAcceptNegative default True;
    {* 是否能输入 - 号}
    property AcceptCharList: string read FAcceptCharList write FAcceptCharList;
    {* 允许输入的字符列表，非 Unicode 环境下对汉字支持不好}

    property OnPaintPadding: TCnPaintPaddingEvent read FOnPaintPadding write FOnPaintPadding;
    {* Padding 不为 0 时触发的自绘 Padding 区的事件}
  end;

implementation

uses
  CnCommon;

{ TCnEdit }

procedure TCnEdit.DoExit;
var
  Ri, Code: Integer;
  Rs: Single;
begin
  inherited;
  // 如果为整形，为空时自动为 0
  if FTextType = IntegerText then
  begin
    if Text = '' then
      Text := '0';
    Val(Text, Ri, Code);
    if Code <> 0 then
      Text := '0';
    Code := Ri; // 避免编译警告
  end;
  // 如果为浮点，检查.前后是否为空，为空加 0
  if FTextType = FloatText then
  begin
    if Pos('.', Text) = 1 then
      Text := '0' + Text;
    if Pos('.', Text) = Length(Text) then
      Text := Text + '0';
    Val(Text, Rs, Code);
    if Code <> 0 then
       Text := '0';
    Code := Round(Rs); // 避免编译警告
  end;
  Invalidate;
end;

procedure TCnEdit.BoundsChanged;
var
  R: TRect;
begin
  SetRect(R, 0, 0, ClientWidth - 2, ClientHeight + 1); // +1 is workAround for Windows paint bug
  if (FLinkStyle <> lsNone) then
    Dec(R.Right, FButtonWidth);
{$IFDEF WIN64}
  SendMessage(Handle, EM_SETRECT, 0, NativeUInt(@R));
{$ELSE}
  SendMessage(Handle, EM_SETRECT, 0, LongInt(@R));
{$ENDIF}
  Repaint;
end;

constructor TCnEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
  FAcceptNegative := True;
end;

procedure TCnEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or ES_MULTILINE;
  end;
end;

destructor TCnEdit.Destroy;
begin
  inherited Destroy;
  FCanvas.Free;
end;

procedure TCnEdit.DoEnter;
begin
  if (FLinkStyle <> lsNone) then
    BoundsChanged;
  inherited DoEnter;
  if AutoSelect then
    SelectAll;
end;

procedure TCnEdit.EditButtonClick;
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self);
end;

function TCnEdit.GetTextMargins: TPoint;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
{$IFDEF FPC}
  SysMetrics, Metrics: Windows.TTextMetric;
{$ELSE}
  SysMetrics, Metrics: TTextMetric;
{$ENDIF}
begin
  if NewStyleControls then
  begin
    if BOrderStyle = bsNone then
      I := 0
{$IFNDEF FPC}
    else if Ctl3D then
      I := 1
{$ENDIF}
    else
      I := 2;
      
    Result.X := SendMessage(Handle, EM_GETMARGINS, 0, 0) and $0000FFFF + I;
    Result.Y := I;
  end
  else
  begin
    if BorderStyle = bsNone then
      I := 0
    else
    begin
      DC := GetDC(0);
      GetTextMetrics(DC, SysMetrics);
      SaveFont := SelectObject(DC, Font.Handle);
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, SaveFont);
      ReleaseDC(0, DC);
      I := SysMetrics.tmHeight;
      if I > Metrics.tmHeight then
        I := Metrics.tmHeight;
      I := I div 4;
    end;
    Result.X := I;
    Result.Y := I;
  end;
end;

procedure TCnEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
{$IFDEF FPC}
  Msg: Messages.TMsg;
{$ELSE}
  Msg: TMsg;
{$ENDIF}
begin
  if (FLinkStyle in [lsEllipsis, lsDropDown]) and (Key = VK_RETURN) and (Shift = [sSCtrl]) then
  begin
    EditButtonClick;
    PeekMessage(Msg, Handle, WM_CHAR, WM_CHAR, PM_REMOVE);
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TCnEdit.KeyPress(var Key: ChAr);
var
  AParent: TControl;
begin
  if Key = #13 then
  begin
    if FEnterAsTab then
    begin
      AParent := Parent;
      if AParent <> nil then
        while AParent.Parent <> nil do
          AParent := AParent.Parent;

      if (AParent <> nil) and (AParent is TControl) then
        (AParent as TControl).Perform(WM_NEXTDLGCTL, 0, 0);
      Key := #0;
    end
    else
      MessageBeep(0);
  end
  else
  begin
    if not CharInSet(Key, [Chr(VK_BACK), Chr(VK_RETURN), #01, #03, #22, #24, #26]) then // Ctrl+A/C/BK/V/X/Z
    begin
      if FTextType = IntegerText then
      begin
        if not FAcceptNegative and (Key = '-') then
          Key := #0
        else if not CharInSet(Key, ['0'..'9', '-']) then
          Key := #0
        else
        begin
          if (Key = '-') and ((Pos('-', Text) > 0) or (SelStart > 0)) then Key := #0;
        end;
      end
      else if FTextType = FloatText then
      begin
        if not FAcceptNegative and (Key = '-') then
          Key := #0
        else if not CharInSet(Key, ['0'..'9', '.', '-']) then
          Key := #0
        else
        begin
          if (Key = '-') and ((Pos('-', Text) > 0) or (SelStart > 0)) then
            Key := #0;
          if (Pos('.', Text) > 0) and (Key = '.') then
            Key := #0;
        end;
      end
      else if (FTextType = IdentText) and (not IsValidIdentChar(Key, SelStart = 0)) then
        Key := #0
      else if FAcceptCharList <> '' then
      begin
        if Pos(Key, FAcceptCharList) = 0 then
          Key := #0;
      end;
    end;
  end;
  inherited KeyPress(Key);
end;

procedure TCnEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (FLinkStyle <> lsNone)
    and PtInRect(Rect(Width - FButtonWidth, 0, Width, Height), Point(X, Y)) then
  begin
    MouseCapture := True;
    FTracking := True;
    TrackButton(X, Y);
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCnEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FTracking then
    TrackButton(X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TCnEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  WasPressed: Boolean;
begin
  WasPressed := FPressed;
  StopTracking;

  if (Button = mbLeft) and (FLinkStyle in [lsEllipsis, lsDropDown]) and WasPressed then
    EditButtonClick;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCnEdit.DoPaintPadding(Canvas: TCanvas; PaddingRect: TRect);
begin
  if Assigned(FOnPaintPadding) then
    FOnPaintPadding(Self, Canvas, PaddingRect);
end;

procedure TCnEdit.SetLinkStyle(Value: TLinkStyle);
begin
  if Value = FLinkStyle then
    Exit;
  FLinkStyle := Value;
  if not HandleAllocated then
    Exit;
  BoundsChanged;
end;

procedure TCnEdit.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TCnEdit.TrackButton(X, Y: Integer);
var
  NewState: Boolean;
  R: TRect;
begin
  SetRect(R, ClientWidth - FButtonWidth, 0, ClientWidth, ClientHeight);
  NewState := PtInRect(R, Point(X, Y));
  if FPressed <> NewState then
  begin
    FPressed := NewState;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TCnEdit.WMPaint(var Message: TWMPaint);
var
  Margins: TPoint;
  R, TR, PR: TRect;
  DC: HDC;
{$IFDEF FPC}
  PS: Windows.TPaintStruct;
{$ELSE}
  PS: TPaintStruct;
{$ENDIF}
  S: string;
  Flags: Integer;
  W: Integer;
  L, I, Offset, D: Integer;
begin
  if FCanvas = nil then
  begin
    FCanvas := TControlCanvas.Create;
    FCanvas.Control := Self;
  end;

  DC := Message.DC;
  if DC = 0 then
    DC := BeginPaint(Handle, PS);

  FCanvas.Handle := DC;
  try
    FCanvas.Font := Font;
    with FCanvas do
    begin
      // 设置控件的范围
      TR := ClientRect;

      if FLinkStyle <> lsNone then // R 是按钮区域
        SetRect(R, ClientWidth - FButtonWidth, 0, ClientWidth, ClientHeight)
      else
      begin
        // TR 是整个区域
        if not (NewStyleControls {$IFNDEF FPC} and Ctl3D {$ENDIF}) and (BOrderStyle = bsSingle) then
        begin
{$IFDEF SUPPORT_THEME}
          if StyleServices.Enabled then
            Brush.Color := StyleServices.GetSystemColor(clWindowFrame)
          else
{$ENDIF}
            Brush.Color := clWindowFrame;

          FrameRect(TR);
          InflateRect(TR, -1, -1);
        end;
        Brush.Color := Color;
      end;

      // 是否是密码型
      S := Text;
      if PasswordChar <> #0 then
        FillChar(S[1], Length(S), PasswordChar);

      // 画文字
      Margins := GetTextMargins;
      if Focused then
      begin
        L := Margins.X;
      end
      else
      begin
        case FAlignment of
          taLeftJustify: L := Margins.X + FPaddingWidth;
          taRightJustify: L := ClientWidth - TextWidth(S) - Margins.X - 1 - FPaddingWidth;
        else
          L := (ClientWidth - TextWidth(S)) div 2;
        end;
      end;

      if FLinkStyle <> lsNone then   // 画按钮
      begin
        // R 是按钮区域
        Flags := 0;
        if FPressed then
          Flags := BF_FLAT;

{$IFDEF SUPPORT_THEME}
        if StyleServices.Enabled then
          Brush.Color := StyleServices.GetStyleColor(scEdit)
        else
{$ENDIF}
          Brush.Color := Color;

        Rectangle(R.Left, R.Top, R.Right, R.Bottom);
        DrawEdge(DC, R, EDGE_RAISED, BF_RECT or BF_MIDDLE or Flags);
        Flags := ((R.Right - R.Left) shr 1) - 1 + Ord(FPressed);
        if FLinkStyle = lsEllipsis then // 画点
        begin
          W := 2; // 点与点之间的横向距离
          PatBlt(DC, R.Left + Flags, R.Top + Round(ClientHeight / 2) - 1, W, W, BLACKNESS);
          PatBlt(DC, R.Left + Flags - (W * 2), R.Top + Round(ClientHeight / 2) - 1, W, W, BLACKNESS);
          PatBlt(DC, R.Left + Flags + (W * 2), R.Top + Round(ClientHeight / 2) - 1, W, W, BLACKNESS);
        end
        else if FLinkStyle = lsDropDown then
        begin
          if FButtonWidth <= 16 then // 常规画法
          begin
            for I := 0 to 3 do // 画下拉箭头
            begin
              // R 高 21 时顶上 7，起始 Y 坐标占三分之一
              // 按钮宽 16 时从 4 开始，11 结束，起始 X 坐标四分之一到四分之三，画的高度是 X 的四分之一
              Windows.MoveToEx(DC, R.Left + 4 + I, R.Top + 7 + I, nil);
              Windows.LineTo(DC, R.Left + 4 + 7 - I, R.Top + 7 + I);
            end;
          end
          else
          begin
            D := FButtonWidth div 4;
            Offset := (FButtonWidth - 16) div 4 - 1;
            if Offset < 0 then
              Offset := 0;
            W := (R.Bottom - R.Top) div 3 + 1;

            for I := 0 to D do
            begin
              if R.Left + D + Offset + I > R.Left + D + Offset + W - I then // 避免画过头
                Break;
              Windows.MoveToEx(DC, R.Left + D + Offset + I, R.Top + W + I, nil);
              Windows.LineTo(DC, R.Left + D + Offset + W - I, R.Top + W + I);
            end;
          end;
        end;

        // 将绘制的按钮区域排除掉，再将 DC 的所有内容画出去
        ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
        PaintWindow(DC);
      end;

{$IFDEF FPC}
      if FLinkStyle = lsNone then // FPC 下貌似 Windows 编辑框会自行绘制内容，此处无需绘制
{$ENDIF}
        TextRect(TR, L, Margins.Y, S); // TR 是整个区域，绘制文字

      // 左右对齐时有 Padding 就让自绘 Padding
      if (FPaddingWidth > 0) and (FAlignment in [taLeftJustify, taRightJustify]) then
      begin
        PR.Top := TR.Top;
        PR.Bottom := TR.Bottom;
        if FAlignment = taLeftJustify then
        begin
          PR.Left := Margins.X;
          PR.Right := L;
        end
        else
        begin
          PR.Left := L;
          PR.Right := L + FPaddingWidth;
        end;

        DoPaintPadding(FCanvas, PR);
      end;
    end;
  finally
    FCanvas.Handle := 0;
    if Message.DC = 0 then
      EndPaint(Handle, PS);
  end;
end;

procedure TCnEdit.WMSetCursor(var Msg: TWMSetCursor);
var
  P: TPoint;
begin
  GetCursorPos(P);
  if (FLinkStyle <> lsNone) and
    PtInRect(Rect(Width - FButtonWidth - 4, 0, ClientWidth,
    ClientHeight), ScreenToClient(P)) then
    Windows.SetCursor(Screen.Cursors[FButtonCursor])
  else
    inherited;
end;

function TCnEdit.GetValue: Variant;
begin
  case FTextType of
    IntegerText: Result := StrToInt(Text);
    FloatText: Result := StrToFloat(Text);
  else
    Result := Text;
  end;
end;

procedure TCnEdit.SetButtonCursor(const Value: TCursor);
begin
  if FButtonCursor <> Value then
  begin
    FButtonCursor := Value;
    Perform(WM_SETCURSOR, 0, 0);
  end;
end;

procedure TCnEdit.SetPaddingWidth(const Value: Integer);
begin
  if Value <> FPaddingWidth then
  begin
    FPaddingWidth := Value;
    Invalidate;
  end;
end;

procedure TCnEdit.SetParent(AParent: TWinControl);
begin
  inherited;
{$IFDEF SUPPORT_THEME}
  if (AParent <> nil) and StyleServices.Enabled then
  begin
    Color := StyleServices.GetStyleColor(scEdit);
    Font.Color := StyleServices.GetStyleFontColor(sfEditBoxTextNormal);
  end;
{$ENDIF};
end;

procedure TCnEdit.WMPaste(var Message: TWMPaste);
var
  I: Integer;
  S: string;
begin
  // 处理粘贴消息，删除不需要的字符，副作用是会影响剪贴板内容
  if Clipboard.AsText = '' then
    Exit;

  S := Clipboard.AsText;
  I := Length(S);
  case FTextType of
    IntegerText:
      begin
        while I > 0 do
        begin
          if not CharInSet(S[I], ['0'..'9', '-']) then
            Delete(S, I, 1);
          Dec(I);
        end;
      end;
    FloatText:
      begin
        while I > 0 do
        begin
          if not CharInSet(S[I], ['0'..'9', '-', '.']) then
            Delete(S, I, 1);
          Dec(I);
        end;
      end;
    IdentText:
      begin
        while I > 0 do
        begin
          if not IsValidIdentChar(S[I], I = 1) then
            Delete(S, I, 1);
          Dec(I);
        end;
      end;  
  else
    ;
  end;
  Clipboard.AsText := S;
  inherited;
end;

end.

