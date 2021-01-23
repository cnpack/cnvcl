{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2021 CnPack 开发组                       }
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
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnMonthCalendar;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包界面组件库
* 单元名称：中国月历组件，能显示农历与干支
* 单元作者：匿名、不夜人
* 备    注：字体尺寸随组件尺寸变化而变化
* 开发平台：PWinXP SP2 + Delphi 2006
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2010.11.08 V1.2
*               修正1582年10月显示不正确的问题
*           2009.04.26 V1.1
*               不夜人加入几种颜色以及前进后退年月的按钮，刘啸修改
*           2008.06.05 V1.0
*               移植单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Controls, Graphics, Windows, Messages,
  StdCtrls, CnCalendar;
  
type
  TCnLunarDate = record //农历日期
    Year: Integer;
    Month: Integer;
    Day: Integer;
    IsLeap: Boolean; //闰月
  end;

  TCnGanZhiDate = record //干支日期
    Year: Integer;
    Month: Integer;
    Day: Integer;
  end;

  TCnMonthCalendar = class;

  TCnCalStyle = (csBottom, csRight, csNone);

  TCnCalColors = class(TPersistent)
  private
    Owner: TCnMonthCalendar;
    FBackColor: TColor;
    FTextColor: TColor;
    FTitleBackColor: TColor;
    FTitleTextColor: TColor;
    FTrailingTextColor: TColor;
    FSundayColor: TColor;
    FSaturdayColor: TColor;
    FWeekTextColor: TColor;       //增加星期的字体颜色属性
    FDaySelectColor: TColor;       //增加选择日期颜色属性
    FDaySelectTextColor: TColor;       //增加选择日期字体颜色属性
    procedure SetColor(Index: Integer; Value: TColor);
  public
    constructor Create(AOwner: TCnMonthCalendar);
    procedure Assign(Source: TPersistent); override;
  published
    property BackColor: TColor index 0 read FBackColor write SetColor default clWindow;
    property TextColor: TColor index 1 read FTextColor write SetColor default clWindowText;
    property TitleBackColor: TColor index 2 read FTitleBackColor write SetColor default clActiveCaption;
    property TitleTextColor: TColor index 3 read FTitleTextColor write SetColor default clWhite;
    property TrailingTextColor: TColor index 4 read FTrailingTextColor write SetColor default clInactiveCaptionText;
    property SundayColor: TColor index 5 read FSundayColor write SetColor default clRed;
    property SaturdayColor: TColor index 6 read FSaturdayColor write SetColor default clMaroon;
    property WeekTextColor: TColor index 7 read FWeekTextColor write SetColor default clActiveCaption; 
    //自己增加星期的字体颜色属性
    property DaySelectColor: TColor index 8 read FDaySelectColor write SetColor default clActiveCaption;
    //自己增加日期选择颜色属性
    property DaySelectTextColor: TColor index 9 read FDaySelectTextColor write SetColor default clWindowText; 
    //自己增加日期选择字体颜色属性
  end;

  TCnMonthCalendar = class(TCustomControl)
  private
    FDate: TDate; //月历当前指向的日期
    FViewDate: TDate;
    FCalColors: TCnCalColors;
    FYear: word;
    FMonth: word;
    FDay: word;
    FFirstDate: TDate; //月历第一格日期
    FTitleRect: TRect; //标题区
    FWeekRect: TRect; //星期区
    FDaysRect: TRect; //日期区
    FOldRect: TRect;
    FNeedUpdate: Boolean;
    FCellWidth: Integer;
    FCellHeight: Integer;
    FShowGanZhi: Boolean; //是否使用干支记日
    FCalStyle: TCnCalStyle;
    FTitleTextSize: Integer;
    FWeekTextSize: Integer;
    FDaySize: Integer;
    FLunarDaySize: Integer;
    FOnChange: TNotifyEvent;
    lblPrevMonth: TLabel;                         //增加的动态建立一个Label
    lblNextMonth: TLabel;                         //增加的动态建立一个Label
    lblPrevYear: TLabel;                         //增加的动态建立一个Label
    lblNextYear: TLabel;                         //增加的动态建立一个Label
    LBTextSize: Integer;
    FShowMonthButton: Boolean;
    FShowYearButton: Boolean;                 //增加的动态建立一个label字体尺寸
    procedure CalcRect; //计算各区以及字体大小
    function CalcDayRect(ADate: TDate): TRect;
    procedure GetFirstDay;
    function GetMaxTextSize(S: string; W, H: Integer): Integer;
    procedure UpdateHighlight(X, Y: Integer);
    procedure SetDate(Value: TDate);
    procedure SetCalColors(Value: TCnCalColors);
    procedure SetCalStyle(Value: TCnCalStyle);
    procedure SetShowGanZhi(Value: Boolean);
    procedure PrevMonthClick(Sender: TObject);  //增加的label单击事件
    procedure NextMonthClick(Sender: TObject);
    procedure PrevYearClick(Sender: TObject);  //增加的label单击事件
    procedure NextYearClick(Sender: TObject);
    procedure SetShowMonthButton(const Value: Boolean);
    procedure SetShowYearButton(const Value: Boolean);  //增加的label单击事件
  protected
    { Protected declarations }
    procedure CreateWnd; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure CMWantSpecialKey(var Message: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure Changed; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //算出农历, 传入公历日期, 返回农历日期
    function ToLunar(TheDate: TDate): TCnLunarDate;
    //求年柱，月柱，日柱TheDate为当天的公历日期)
    function GetGanZhi(TheDate: TDate): TCnGanZhiDate;
    //取汉字日期
    function FormatLunarDay(Day: Integer): string;
    //汉字月份
    function FormatLunarMonth(Month: Integer; isLeap: Boolean): string;
    //汉字年份
    function FormatLunarYear(Year: Integer): string;
    // 取得指定日期的节气
    function GetJieQi(TheDate: TDate): string;

    property Year: word read FYear;
    property Month: word read FMonth;
    property Day: word read FDay;
    procedure PriorYear;
    procedure NextYear;
    procedure PriorMonth;
    procedure NextMonth;
    procedure PriorDay;
    procedure NextDay;
    procedure PriorWeek;
    procedure NextWeek;
    procedure FirstDayOfMonth;
    procedure LastDayOfMonth;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BorderWidth;
    property CalColors: TCnCalColors read FCalColors write SetCalColors;
    property CalStyle: TCnCalStyle read FCalStyle write SetCalStyle default csBottom;
    property ShowGanZhi: Boolean read FShowGanZhi write SetShowGanZhi default False;
    property ShowMonthButton: Boolean read FShowMonthButton write SetShowMonthButton;
    property ShowYearButton: Boolean read FShowYearButton write SetShowYearButton;
    property Cursor;
    property Date: TDate read FDate write SetDate;
    property Enabled;
    property Font;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

const
  LunarStrs: array[0..10] of string =
  ('日', '一', '二', '三', '四', '五', '六', '七', '八', '九', '十');

constructor TCnMonthCalendar.Create(AOwner: TComponent);

  procedure InitLabel(ALabel: TLabel);
  begin
    ALabel.Parent := Self;
    ALabel.Visible := False;
    ALabel.Left := 10;
    ALabel.Top := 20;
    ALabel.Transparent := True;
    ALabel.Font.Size := 12;
    ALabel.Font.Style := [fsBold];
  end;
begin
  inherited;
  DoubleBuffered := True;
  ControlStyle := (ControlStyle - [csAcceptsControls, csNoStdEvents, csSetCaption]) + [csReflector];
  FDate := SysUtils.Date;
  FViewDate := FDate;
  FCalColors := TCnCalColors.Create(self);
  GetFirstDay;
  Width := 360;
  Height := 240;
  Font.Name := '宋体';
  Font.Charset := GB2312_CHARSET;
  Font.Size := 9;
  TabStop := True;
  Color := FCalColors.BackColor;
  FShowGanZhi := False;
  //DoubleBuffered := False;

  lblPrevMonth := TLabel.Create(Self);
  InitLabel(lblPrevMonth);
  lblPrevMonth.Caption := '<';
  lblPrevMonth.OnClick := PrevMonthClick;

  lblNextMonth := TLabel.Create(Self);
  InitLabel(lblNextMonth);
  lblNextMonth.Caption := '>';
  lblNextMonth.OnClick := NextMonthClick;

  lblPrevYear := TLabel.Create(Self);
  InitLabel(lblPrevYear);
  lblPrevYear.Caption := '<<';
  lblPrevYear.OnClick := PrevYearClick;

  lblNextYear := TLabel.Create(Self);
  InitLabel(lblNextYear);
  lblNextYear.Caption := '>>';
  lblNextYear.OnClick := NextYearClick;

end;

procedure TCnMonthCalendar.PrevMonthClick(Sender:   TObject);
begin
  PriorMonth;
end;

procedure TCnMonthCalendar.NextMonthClick(Sender:   TObject);
begin
  NextMonth;
end;

destructor TCnMonthCalendar.Destroy;
begin
  FCalColors.Free;
  lblPrevMonth.Free;   //增加的释放动态建立的label
  lblNextMonth.Free;  //增加的释放动态建立的label
  lblPrevYear.Free;
  lblNextYear.Free;
  inherited Destroy;
end;

procedure TCnMonthCalendar.CreateWnd;
begin
  inherited;
  CalcRect;
  Color := FCalColors.BackColor;
end;

procedure TCnMonthCalendar.Paint;
var
  OutputStr: string;
  Col, I, Skip: Integer;
  TempDate: TDate;
  R, DR: TRect;
  Y, M, D: word;
  GzDate: TCnGanZhiDate;

  procedure DrawString(const S: string; Bounds: TRect; Flag: Cardinal);
  var
    TextSize: TSize;
    StartPos: TPoint;
  begin
    TextSize := Canvas.TextExtent(S);
    StartPos := Bounds.TopLeft;
    with StartPos, Bounds, TextSize do
    begin
      if (DT_CENTER and Flag) = DT_CENTER then X := X + (Right - Left - cx) div 2
      else if (DT_RIGHT and Flag) = DT_RIGHT then X := X + (Right - Left - cx);

      if (DT_VCENTER and Flag) = DT_VCENTER then Y := Y + (Bottom - Top - cy) div 2
      else if (DT_BOTTOM and Flag) = DT_BOTTOM then Y := Y + (Bottom - Top - cy);
        Canvas.TextOut(X, Y, S);
    end;
  end;

  procedure DrawLunarDay(R: TRect; TheDate: TDate);
  var
    S, S1: string;
    LunarDate: TCnLunarDate;
    H, yy: Integer;
  begin
    S := GetJieQi(TheDate);
    if S = '' then
      if FShowGanZhi then
      begin
        GzDate := GetGanZhi(TheDate);
        S := GetGanZhiFromNumber(GzDate.Day);
      end
      else
      begin
        LunarDate := ToLunar(TheDate);
        if LunarDate.Day = 0 then Exit;
        if LunarDate.Day = 1 then
          S := FormatLunarMonth(LunarDate.Month, LunarDate.isLeap)
        else
          S := FormatLunarDay(LunarDate.Day);
      end;
    Canvas.Font.Size := FLunarDaySize;
    if FCalStyle = csRight then
    begin
      H := Canvas.TextHeight(S);
      yy := R.Top + (FCellHeight div 2) - H;
      S1 := Copy(S, 1, 2);
        Canvas.TextOut(R.Left + 2, yy, S1);
      yy := yy + H;
      S1 := Copy(S, 3, 2);
        Canvas.TextOut(R.Left + 2, yy, S1);
    end
    else
      DrawString(S, R, DT_TOP or DT_CENTER);
  end;

begin 
  inherited;
  Canvas.Font.Assign(Font);
    with Canvas, FCalColors do
    begin
      // 画年月
      if ShowYearButton then
      begin
        lblPrevYear.Font.Size := LBTextSize;
        lblPrevYear.Font.Color := TitleTextColor;
        lblNextYear.Font.Size := LBTextSize;
        lblNextYear.Font.Color := TitleTextColor;
      end;
      if ShowMonthButton then
      begin
        lblPrevMonth.Font.Size := LBTextSize;
        lblPrevMonth.Font.Color := TitleTextColor;
        lblNextMonth.Font.Size := LBTextSize;
        lblNextMonth.Font.Color := TitleTextColor;
      end;  

      if RectVisible(Canvas.Handle, FTitleRect) then
      begin
        Brush.Color := TitleBackColor;
        Brush.Style := bsSolid;
        FillRect(FTitleRect);
        Brush.Style := bsClear;
        Font.Color := TitleTextColor;
        Font.Size := FTitleTextSize;
        Font.Style := [fsBold];
        if FShowGanZhi then
        begin
          GzDate := GetGanZhi(FDate);
          OutputStr := GetGanZhiFromNumber(GzDate.Year) + '(' + GetShengXiaoFromNumber(GzDate.Year mod 12) +
            ')年' + GetGanZhiFromNumber(GzDate.Month) + '月';
        end
        else
          OutputStr := FormatDateTime('yyyy', FDate) + '年' + FormatDateTime('m', FDate) + '月';
        DrawString(OutputStr, FTitleRect, DT_CENTER or DT_VCENTER);
        Font.Style := [];
      end;
      //画星期
      R := Bounds(FWeekRect.Left, FWeekRect.Top, FCellWidth, FCellHeight);
      if RectVisible(Canvas.Handle, FWeekRect) then
      begin
        Font.Size := FWeekTextSize;
        Font.Color := WeekTextColor;     //增加上的，改变星期头字体颜色
        for I := 0 to 6 do
        begin
          OutputStr := GetWeekFromNumber(GetWeek(FFirstDate + I));
          DrawString(OutputStr, R, DT_CENTER or DT_VCENTER);
          OffsetRect(R, FCellWidth, 0);
        end;
        Pen.Color := TitleBackColor;
        Pen.Width := 1;
        Pen.Mode := pmCopy;
        PenPos := Point(2, FWeekRect.Bottom - 2);
        LineTo(FWeekRect.Right - 2, FWeekRect.Bottom - 2);
      end;
      //画日期
      R := Bounds(FDaysRect.Left, FDaysRect.Top, FCellWidth, FCellHeight);
      Skip := 0;
      for I := 0 to 41 do
      begin
        Col := (I - Skip) mod 7;

        //if RectVisible(Canvas.Handle, R) then  // NOTE: NEVER!

        TempDate := FFirstDate + I;
        DecodeDate(TempDate, Y, M, D);

        if (Y = 1582) and (M = 10) and (D in [5..14]) then
        begin
          Inc(Skip);
          Continue;
        end;

        if M = FMonth then
          if Col = 0 then
            Font.Color := SundayColor
          else if Col = 6 then
            Font.Color := SaturdayColor
          else
            Font.Color := TextColor
        else
          Font.Color := TrailingTextColor;

        if Trunc(TempDate) = Trunc(FViewDate) then //高亮显示月历日期
        begin
          Brush.Color := DaySelectColor;   // 增加的颜色设置
          Font.Color := DaySelectTextColor;    // 增加的颜色设置
          FillRect(R);
          FOldRect := R;
          DR := R;
          InflateRect(DR, -2, -2);
          if Focused then
            Windows.DrawFocusRect(Handle, DR);
        end
        else
        begin
          Brush.Color := Color;
          Brush.Style := bsSolid;
          FillRect(R);
        end;

        Brush.Style := bsClear;

        if TempDate = SysUtils.Date then //在当前日期画一红色框
        begin
          Pen.Color := clRed;
          Pen.Width := 1;
          Rectangle(R.Left, R.Top, R.Right, R.Bottom);
        end;
        OutputStr := IntToStr(D);
        Font.Size := FDaySize;
        if FCalStyle = csNone then
          DrawString(OutputStr, R, DT_VCENTER or DT_CENTER)
        else
        begin
          if FCalStyle = csRight then
          begin
            DR := Bounds(R.Left, R.Top, FCellWidth div 3 * 2, FCellHeight);
            DrawString(OutputStr, DR, DT_VCENTER or DT_RIGHT);
            OffsetRect(DR, FCellWidth div 3 * 2, 0);
          end
          else
          begin
            DR := Bounds(R.Left, R.Top, FCellWidth, FCellHeight div 5 * 3);
            DrawString(OutputStr, DR, DT_BOTTOM or DT_CENTER);
            DR := Bounds(DR.Left, DR.Bottom, FCellWidth, FCellHeight div 5 * 2);
          end;
          DrawLunarDay(DR, TempDate);
        end;

        if Col = 6 then
          OffsetRect(R, FDaysRect.Left - R.Left, FCellHeight)
        else
          OffsetRect(R, FCellWidth, 0);
      end; {end for}
    end; {end with}
end;

procedure TCnMonthCalendar.Resize;
begin
  inherited;
  CalcRect;
end;

procedure TCnMonthCalendar.CalcRect;
begin
  Canvas.Font.Assign(Font);
  FCellWidth := ClientRect.Right div 7;
  FCellHeight := ClientRect.Bottom div 8;
  FTitleRect := ClientRect;
  FTitleRect.Bottom := FCellHeight;
  FTitleTextSize := GetMaxTextSize(FormatDateTime('yyyy年mm月', FDate), FTitleRect.Right, Round((FTitleRect.Bottom - FTitleRect.Top) * 0.8));
  LBTextSize := Round(FTitleTextSize * 0.8);       //增加的label字体尺寸

  FWeekTextSize := GetMaxTextSize(FormatDateTime('ddd', FDate), Round(FCellWidth * 1.2), FCellHeight);       //这是修改后的字体(加大了！)

  if FCalStyle = csNone then
    FDaySize := GetMaxTextSize(FormatDateTime('dd', FDate), FCellWidth, FCellHeight)
  else begin
    if FCalStyle = csRight then
    begin
      FDaySize := GetMaxTextSize(FormatDateTime('dd', FDate), FCellWidth div 3 * 2, FCellHeight);
      FLunarDaySize := GetMaxTextSize('九', FCellWidth div 3, FCellHeight div 2);
    end
    else begin
      FDaySize := GetMaxTextSize(FormatDateTime('dd', FDate), FCellWidth, FCellHeight div 5 * 3);
      FLunarDaySize := GetMaxTextSize('九九', FCellWidth, FCellHeight div 5 * 2);
    end;
  end;
  FWeekRect := Bounds(0, FTitleRect.Bottom, FCellWidth * 7, FCellHeight);

  FDaysRect := Bounds(0, FWeekRect.Bottom, FCellWidth * 7, FCellHeight * 6);

  lblPrevMonth.Font.Size := LBTextSize;
  lblPrevMonth.Font.Color := FCalColors.TitleTextColor;
  lblPrevMonth.Left := 40;
  lblPrevMonth.Top := Round((FTitleRect.Bottom - lblPrevMonth.Height) / 2);

  lblNextMonth.Font.Size := LBTextSize;
  lblNextMonth.Font.Color := FCalColors.TitleTextColor;
  lblNextMonth.Left := FTitleRect.Right - 30 - Round(LBTextSize * 1.2);
  lblNextMonth.Top := lblPrevMonth.Top;

  lblPrevYear.Font.Size := LBTextSize;
  lblPrevYear.Font.Color := FCalColors.TitleTextColor;
  lblPrevYear.Left := 10;
  lblPrevYear.Top := Round((FTitleRect.Bottom - lblPrevYear.Height) / 2);

  lblNextYear.Font.Size := LBTextSize;
  lblNextYear.Font.Color := FCalColors.TitleTextColor;
  lblNextYear.Left := FTitleRect.Right - 10 - Round(LBTextSize * 1.2);
  lblNextYear.Top := lblPrevYear.Top;
end;

function TCnMonthCalendar.CalcDayRect(ADate: TDate): TRect;
var
  DateOffset: Integer;
  col, Row: Integer;
begin
  DateOffset := Trunc(Abs(ADate - FFirstDate));
  Row := DateOffset div 7;
  col := DateOffset mod 7;
  Result.Left := FDaysRect.Left + FCellWidth * col;
  Result.Top := FDaysRect.Top + FCellHeight * Row;
  Result.Right := Result.Left + FCellWidth;
  Result.Bottom := Result.Top + FCellHeight;
end;

function TCnMonthCalendar.GetMaxTextSize(S: string; W, H: Integer): Integer;
var
  n: Integer;
  TextSize: TSize;
begin
  for n := 5 to 72 do
  begin
    Canvas.Font.Size := n;
    TextSize := Canvas.TextExtent(S);
    if (TextSize.cx > W) or (TextSize.cy > H) then Break;
  end;
  Result := n - 1;
end;

procedure TCnMonthCalendar.GetFirstDay;
var
  DayOffSet: Integer;
begin
  DecodeDate(FDate, FYear, FMonth, FDay);
  FFirstDate := EncodeDate(FYear, FMonth, 1);
  DayOffSet := GetWeek(FFirstDate) + 1;
  if DayOffSet = 1 then DayOffSet := 8; //保证前面包含上月数据
  FFirstDate := FFirstDate + 1 - DayOffSet;
end;

procedure TCnMonthCalendar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not (csDesigning in ComponentState) then SetFocus;
  if Button = mbLeft then
  begin
    UpdateHighlight(X, Y);
  end;
end;

procedure TCnMonthCalendar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssLeft in Shift then
    UpdateHighlight(X, Y);
end;

procedure TCnMonthCalendar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
  begin
    FNeedUpdate := False;
    SetDate(FViewDate);
  end;
end;

procedure TCnMonthCalendar.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  inherited;
  if Message.CharCode in [vk_Left..vk_Down] then Message.Result := 1;
end;

procedure TCnMonthCalendar.KeyDown(var Key: word; Shift: TShiftState);
var
  D, m, Y: word;
begin
  inherited;
  if Shift = [] then
  begin
    FNeedUpdate := True;
    case Key of
      vk_Up: SetDate(FDate - 7);
      vk_Down: SetDate(FDate + 7);
      vk_Left: SetDate(FDate - 1);
      vk_Right: SetDate(FDate + 1);
      vk_Home: begin
          DecodeDate(FDate, Y, m, D);
          SetDate(EncodeDate(Y, m, 1));
        end;

      vk_End: begin
          DecodeDate(IncMonth(FDate, 1), Y, m, D);
          SetDate(EncodeDate(Y, m, 1) - 1);
        end;
      vk_Prior: SetDate(IncMonth(FDate, -1));
      vk_Next: SetDate(IncMonth(FDate, 1));
    end;

    if Key = vk_Return then
      inherited Click;
  end;
end; {= TCnMonthCalendar.KeyDown =}

procedure TCnMonthCalendar.UpdateHighlight(X, Y: Integer);
var
  Col, Row: Integer;
  TempDate: TDate;
  R: TRect;
  Ye, M, D: Word;
begin
  if PtInRect(FDaysRect, Point(X, Y)) then
  begin
    Col := X div FCellWidth;
    Row := (Y - FDaysRect.Top) div FCellHeight;
    TempDate := FFirstDate + Col + Row * 7;

    DecodeDate(TempDate, Ye, M, D);
    if (Ye = 1582) and (M = 10) and (D in [5..31]) then
    begin
      DecodeDate(FViewDate, Ye, M, D);
      if M = 10 then
        TempDate := TempDate + 10;
    end;

    if TempDate <> FViewDate then
    begin
      R := Bounds(FDaysRect.Left + FCellWidth * Col + 1,
        FDaysRect.Top + FCellHeight * Row + 1, FCellWidth - 2, FCellHeight - 2);
      FViewDate := TempDate;
      InvalidateRect(Handle, @FOldRect, False);
      InvalidateRect(Handle, @R, False);
    end;
  end;
end;

procedure TCnMonthCalendar.DoEnter;
begin
  inherited;
  with Canvas, FCalColors do
  begin
    Brush.Style := bsSolid;
    Brush.Color := TitleBackColor;
    Font.Color := TitleTextColor;
    Windows.DrawFocusRect(Handle, FOldRect);
  end;
end;

procedure TCnMonthCalendar.DoExit;
begin
  inherited;
  with Canvas, FCalColors do
  begin
    Brush.Style := bsSolid;
    Brush.Color := TitleBackColor;
    Font.Color := TitleTextColor;
    Windows.DrawFocusRect(Handle, FOldRect);
  end;
end;

procedure TCnMonthCalendar.SetCalColors(Value: TCnCalColors);
begin
  if FCalColors <> Value then FCalColors.Assign(Value);
end;

procedure TCnMonthCalendar.SetCalStyle(Value: TCnCalStyle);
begin
  if FCalStyle <> Value then
  begin
    FCalStyle := Value;
    CalcRect;
    Invalidate;
  end;
end;

procedure TCnMonthCalendar.SetDate(Value: TDate);
var
  oldFirstDate: TDate;
  R: TRect;
begin
  if (FDate <> Trunc(Value)) then
  begin
    FDate := Value;
    FViewDate := FDate;
    oldFirstDate := FFirstDate;
    GetFirstDay;
    Changed;
    FNeedUpdate := True;

    if oldFirstDate <> FFirstDate then
    begin
      InvalidateRect(Handle, @FTitleRect, False);
      InvalidateRect(Handle, @FDaysRect, False);
    end
    else if FNeedUpdate then
    begin
      InvalidateRect(Handle, @FOldRect, False);
      R := CalcDayRect(FViewDate);
      InvalidateRect(Handle, @R, False);
    end
    else if FShowGanZhi then
    begin
      InvalidateRect(Handle, @FTitleRect, False);
    end;
  end;
end; {= TCnMonthCalendar.SetDate =}

procedure TCnMonthCalendar.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

//算出农历, 传入公历日期, 返回农历日期
function TCnMonthCalendar.ToLunar(TheDate: TDate): TCnLunarDate;
var
  Y, M, D: Word;
begin
  DecodeDate(TheDate, Y, M, D);
  GetLunarFromDay(Y, M, D, Result.Year, Result.Month, Result.Day, Result.IsLeap);
  Result.Year := Y;
end;

// 求年柱，月柱，日柱, TheDate 为公历日期
function TCnMonthCalendar.GetGanZhi(TheDate: TDate): TCnGanZhiDate;
var
  Y, M, D: Word;
begin
  DecodeDate(TheDate, Y, M, D);
  Result.Year := GetGanZhiFromYear(Y, M, D);
  Result.Month := GetGanZhiFromMonth(Y, M, D);
  Result.Day := GetGanZhiFromDay(Y, M, D);
end;

function TCnMonthCalendar.FormatLunarDay(Day: Integer): string;
begin
  case Day of
    1..10: Result := SCnLunarNumber2Array[0] + LunarStrs[Day];
    11..19: Result := SCnLunarNumber2Array[1] + LunarStrs[Day - 10];
    20: Result := LunarStrs[2] + LunarStrs[10];
    21..29: Result := SCnLunarNumber2Array[2] + LunarStrs[Day - 20];
    30: Result := LunarStrs[3] + LunarStrs[10];
  else Result := '';
  end;
end;

function TCnMonthCalendar.FormatLunarMonth(Month: Integer; isLeap: Boolean): string;
begin
  case Month of
    1..10: Result := LunarStrs[Month];
    11..12: Result := LunarStrs[10] + LunarStrs[Month - 10];
  else Result := '';
  end;
  if isLeap then Result := '闰' + Result;
  Result := Result + '月';
end;

function TCnMonthCalendar.FormatLunarYear(Year: Integer): string;
var
  temp: Integer;
  zero: string;
begin
  zero := '零';

  temp := Year div 1000;
  Result := LunarStrs[temp];
  Year := Year - temp * 1000;

  if Year >= 100 then
  begin
    temp := Year div 100;
    Result := Result + LunarStrs[temp];
    Year := Year - temp * 100;
  end
  else
    Result := Result + zero;

  if Year >= 10 then
  begin
    temp := Year div 10;
    Result := Result + LunarStrs[temp];
    Year := Year - temp * 10;
  end
  else
    Result := Result + zero;

  if Year = 0 then Result := Result + zero else
    Result := Result + LunarStrs[Year];
  Result := Result + '年';
end;

// 取得指定日期的节气
function TCnMonthCalendar.GetJieQi(TheDate: TDate): string;
var
  Y, M, D: Word;
  J: Integer;
begin
  Result := '';
  DecodeDate(TheDate, Y, M, D);
  J := GetJieQiFromDay(Y, M, D);
  if J <> -1 then
    Result := SCnJieQiArray[J];
end;

{ TCnCalColors }

constructor TCnCalColors.Create(AOwner: TCnMonthCalendar);
begin
  Owner := AOwner;
  FBackColor := clWindow;
  FTextColor := clWindowText;
  FTitleBackColor := clActiveCaption;
  FTitleTextColor := clWhite;
  FTrailingTextColor := clInactiveCaptionText;
  FSundayColor := clRed;
  FSaturdayColor := clMaroon;
  FWeekTextColor := clActiveCaption;    // 增加星期字头颜色
  FDaySelectColor := clActiveCaption;    // 增加选择日期的填充颜色颜色
  FDaySelectTextColor := clWhite;    // 增加选择日期字体的填充颜色颜色
end;

procedure TCnCalColors.SetColor(Index: Integer; Value: TColor);
begin
  case Index of
    0: FBackColor := Value;
    1: FTextColor := Value;
    2: FTitleBackColor := Value;
    3: FTitleTextColor := Value;
    4: FTrailingTextColor := Value;
    5: FSundayColor := Value;
    6: FSaturdayColor := Value;
    7: FWeekTextColor := Value;   // 增加星期字头字头颜色
    8: FDaySelectColor := Value;   // 增加选择日期的填充颜色颜色
    9: FDaySelectTextColor := Value;   // 增加选择日期字体的填充颜色颜色
  end;
  if Owner.HandleAllocated then
  begin
    Owner.Color := FBackColor;
    Owner.Invalidate;
  end;
end;

procedure TCnCalColors.Assign(Source: TPersistent);
begin
  if (Source = nil) or not (Source is TCnCalColors) then Exit;
  FBackColor := TCnCalColors(Source).BackColor;
  FTextColor := TCnCalColors(Source).TextColor;
  FTitleBackColor := TCnCalColors(Source).TitleBackColor;
  FTitleTextColor := TCnCalColors(Source).TitleTextColor;
  FTrailingTextColor := TCnCalColors(Source).TrailingTextColor;
  FSundayColor := TCnCalColors(Source).SundayColor;
  FSaturdayColor := TCnCalColors(Source).SaturdayColor;
  FWeekTextColor := TCnCalColors(Source).WeekTextColor;  // 增加星期字头字头颜色
  FDaySelectColor := TCnCalColors(Source).DaySelectColor;
  FDaySelectTextColor := TCnCalColors(Source).DaySelectTextColor;
end;

procedure TCnMonthCalendar.SetShowGanZhi(Value: Boolean);
begin
  if Value <> FShowGanZhi then
  begin
    FShowGanZhi := Value;
    Invalidate;
  end;
end;

procedure TCnMonthCalendar.FirstDayOfMonth;
var
  D, m, Y: word;
begin
  FNeedUpdate := True;
  DecodeDate(FDate, Y, m, D);
  SetDate(EncodeDate(Y, m, 1));
end;

procedure TCnMonthCalendar.LastDayOfMonth;
var
  D, m, Y: word;
begin
  FNeedUpdate := True;
  DecodeDate(IncMonth(FDate, 1), Y, m, D);
  SetDate(EncodeDate(Y, m, 1) - 1);
end;

procedure TCnMonthCalendar.NextDay;
begin
  FNeedUpdate := True;
  SetDate(FDate + 1);
end;

procedure TCnMonthCalendar.NextMonth;
begin
  FNeedUpdate := True;
  SetDate(IncMonth(FDate, 1));
end;

procedure TCnMonthCalendar.NextYear;
begin
  FNeedUpdate := True;
  SetDate(IncMonth(FDate, 12));
end;

procedure TCnMonthCalendar.PriorDay;
begin
  FNeedUpdate := True;
  SetDate(FDate - 1);
end;

procedure TCnMonthCalendar.PriorYear;
begin
  FNeedUpdate := True;
  SetDate(IncMonth(FDate, -12));
end;

procedure TCnMonthCalendar.PriorMonth;
begin
  FNeedUpdate := True;
  SetDate(IncMonth(FDate, -1));
end;

procedure TCnMonthCalendar.NextWeek;
begin
  FNeedUpdate := True;
  SetDate(FDate + 7);
end;

procedure TCnMonthCalendar.PriorWeek;
begin
  FNeedUpdate := True;
  SetDate(FDate - 7);
end;

procedure TCnMonthCalendar.SetShowMonthButton(const Value: Boolean);
begin
  FShowMonthButton := Value;
  lblPrevMonth.Visible := Value;
  lblNextMonth.Visible := Value;
end;

procedure TCnMonthCalendar.SetShowYearButton(const Value: Boolean);
begin
  FShowYearButton := Value;
  lblNextYear.Visible := Value;
  lblPrevYear.Visible := Value;
end;

procedure TCnMonthCalendar.NextYearClick(Sender: TObject);
begin
  NextYear;
end;

procedure TCnMonthCalendar.PrevYearClick(Sender: TObject);
begin
  PriorYear;
end;

end.
