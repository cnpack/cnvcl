unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, CnMonthCalendar;

type
  TFormCalendar = class(TForm)
    pgc1: TPageControl;
    ts1: TTabSheet;
    lblYear: TLabel;
    lblYue: TLabel;
    lblDay: TLabel;
    lblHour: TLabel;
    mmoResult: TMemo;
    edtYear: TEdit;
    edtMonth: TEdit;
    edtDay: TEdit;
    btnCalc: TButton;
    edtHour: TEdit;
    Button1: TButton;
    tsCalendar: TTabSheet;
    CnMonthCalendar1: TCnMonthCalendar;
    chkGanZhi: TCheckBox;
    tsConvert: TTabSheet;
    lbl1: TLabel;
    edtLunarYear: TEdit;
    lbl2: TLabel;
    edtLunarMonth: TEdit;
    lbl3: TLabel;
    edtLunarDay: TEdit;
    chkLeap: TCheckBox;
    btnCalcLunar: TButton;
    mmoLunar: TMemo;
    chkMonthButton: TCheckBox;
    chkYearButton: TCheckBox;
    dtpSet: TDateTimePicker;
    lblDate: TLabel;
    tsSun: TTabSheet;
    lblSunYear: TLabel;
    edtSunYear: TEdit;
    edtSunMonth: TEdit;
    edtSunDay: TEdit;
    lblSunDay: TLabel;
    lblSunMonth: TLabel;
    lblLongi: TLabel;
    edtSunLongi: TEdit;
    lblLat: TLabel;
    edtSunLat: TEdit;
    btnSunTime: TButton;
    btnSunAngle: TButton;
    mmoSun: TMemo;
    tsDays: TTabSheet;
    btnEquStandardDays: TButton;
    btnJulianDays: TButton;
    btnCheckDays: TButton;
    btnEquStandardDays1: TButton;
    mmoDays: TMemo;
    btnCheckLunar: TButton;
    btnVerifyLeapNumber: TButton;
    btnCheckAll: TButton;
    procedure btnCalcClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure chkGanZhiClick(Sender: TObject);
    procedure btnCalcLunarClick(Sender: TObject);
    procedure chkMonthButtonClick(Sender: TObject);
    procedure chkYearButtonClick(Sender: TObject);
    procedure dtpSetChange(Sender: TObject);
    procedure CnMonthCalendar1Change(Sender: TObject);
    procedure btnSunTimeClick(Sender: TObject);
    procedure btnSunAngleClick(Sender: TObject);
    procedure btnEquStandardDaysClick(Sender: TObject);
    procedure btnJulianDaysClick(Sender: TObject);
    procedure btnCheckDaysClick(Sender: TObject);
    procedure btnEquStandardDays1Click(Sender: TObject);
    procedure btnCheckLunarClick(Sender: TObject);
    procedure btnVerifyLeapNumberClick(Sender: TObject);
    procedure btnCheckAllClick(Sender: TObject);
  private
    procedure ConvertEditToDate;
  public

  end;

var
  FormCalendar: TFormCalendar;

implementation

uses
  CnCalendar, CnCalClass;

{$R *.DFM}

var
  AYear, AMonth, ADay, AHour: Integer;

procedure TFormCalendar.btnCalcClick(Sender: TObject);
var
  GZYear, GZMonth: Integer;
  I, GanZhi, Gan, Zhi, JiuXing: Integer;
  M1, D1, H1, mi1: Integer;
  M2, D2, H2, mi2: Integer;
  v91, v92: Integer;
  vf1, vf2: Integer;
  rmMonth, rmDay, cmMonth, cmDay: Integer;
  LunarYear, LunarMonth, LunarDay: Integer;
  IsLeap: Boolean;
  FSeq, FDay: Integer;
begin
  mmoResult.Clear;
  ConvertEditToDate;
  ValidDate(AYear, AMonth, ADay);
  
  mmoResult.Lines.Add('公历日数：' + IntToStr(GetAllDays(AYear, AMonth, ADay)));
  mmoResult.Lines.Add('等效标准日数：' + IntToStr(GetEquStandardDays(AYear, AMonth, ADay)));
  mmoResult.Lines.Add('儒略日数：' + FloatToStr(GetJulianDate(AYear, AMonth, ADay)));
  mmoResult.Lines.Add('约化儒略日数：' + FloatToStr(GetModifiedJulianDate(AYear, AMonth, ADay)));
  mmoResult.Lines.Add('星期：' + IntToStr(GetWeek(AYear, AMonth, ADay)));
  if GetShu9Day(AYear, AMonth, ADay, v91, v92) then
    mmoResult.Lines.Add(Format('%d九第%d天', [v91, v92]));
  if Get3FuDay(AYear, AMonth, ADay, vf1, vf2) then
    mmoResult.Lines.Add(Format('%d伏第%d天', [vf1, vf2]));
  mmoResult.Lines.Add('星座：' + GetXingZuoFromNumber(GetXingZuoFromMonthDay(AMonth, ADay)));

  mmoResult.Lines.Add('年三元：' + Get3YuanFromNumber(Get3YuanFromYear(AYear, AMonth, ADay)));
  mmoResult.Lines.Add('运九星：' + Get9XingFromNumber(GetYun9XingFromYear(AYear, AMonth, ADay)));
  GanZhi := GetGanZhiFromYear(AYear, AMonth, ADay, AHour);
  ExtractGanZhi(GanZhi, Gan, Zhi);
  JiuXing := Get9XingFromYear(AYear, AMonth, ADay);

  GZYear := AYear;
  GZMonth := AMonth;
  AdjustYearMonthToGanZhi(GZYear, GZMonth, ADay, AHour);
  mmoResult.Lines.Add('干支纪年：' + IntToStr(GZYear) + '年' + IntToStr(GZMonth) + '月');
  mmoResult.Lines.Add('生肖：' + GetShengXiaoFromNumber(Zhi));
  mmoResult.Lines.Add('年：'+ GetGanZhiFromNumber(GanZhi) + '，五行：'
    + Get5XingFromNumber(Get5XingFromGan(Gan)) + Get5XingFromNumber(Get5XingFromZhi(Zhi))
    + '，九星：' + Get9XingFromNumber(JiuXing) + '，值年太岁：' + Get60TaiSuiFromNumber(GanZhi));

  GanZhi := GetGanZhiFromMonth(AYear, AMonth, ADay, AHour);
  ExtractGanZhi(GanZhi, Gan, Zhi);
  JiuXing := Get9XingFromMonth(AYear, AMonth, ADay);
  mmoResult.Lines.Add('月：'+ GetGanZhiFromNumber(GanZhi) + '，五行：'
    + Get5XingFromNumber(Get5XingFromGan(Gan)) + Get5XingFromNumber(Get5XingFromZhi(Zhi))
    + '，九星：' + Get9XingFromNumber(JiuXing));

  GanZhi := GetGanZhiFromDay(AYear, AMonth, ADay, AHour);
  ExtractGanZhi(GanZhi, Gan, Zhi);
  JiuXing := Get9XingFromDay(AYear, AMonth, ADay);
  mmoResult.Lines.Add('日：'+ GetGanZhiFromNumber(GanZhi) + '，五行：'
    + Get5XingFromNumber(Get5XingFromGan(Gan)) + Get5XingFromNumber(Get5XingFromZhi(Zhi))
    + '，九星：' + Get9XingFromNumber(JiuXing));

  GanZhi := GetGanZhiFromHour(AYear, AMonth, ADay, AHour);
  ExtractGanZhi(GanZhi, Gan, Zhi);
  JiuXing := Get9XingFromHour(AYear, AMonth, ADay, AHour);
  mmoResult.Lines.Add('时：'+ GetGanZhiFromNumber(GanZhi) + '，五行：'
    + Get5XingFromNumber(Get5XingFromGan(Gan)) + Get5XingFromNumber(Get5XingFromZhi(Zhi))
    + '，九星：' + Get9XingFromNumber(JiuXing));
  mmoResult.Lines.Add('六曜日：' + Get6YaoFromNumber(Get6YaoFromDay(AYear, AMonth, ADay)));
  mmoResult.Lines.Add('时辰：' + GetDiZhiFromNumber(GetShiChenFromHour(AHour)));
  mmoResult.Lines.Add('二十八宿：'+ Get28XiuFromNumber(Get28XiuFromDay(AYear, AMonth, ADay)) + '/' + Get28XiuLongFromNumber(Get28XiuFromDay(AYear, AMonth, ADay)));
  mmoResult.Lines.Add('本日纳音五行：'+ Get5XingFromNumber(Get5XingFromDay(AYear, AMonth, ADay)) + '/' + Get5XingLongFromDay(AYear, AMonth, ADay));
  mmoResult.Lines.Add('十二建：'+ Get12JianFromNumber(Get12JianFromDay(AYear, AMonth, ADay)));

  mmoResult.Lines.Add('吉神方位：');
  mmoResult.Lines.Add('财神' + GetJiShenFangWeiFromNumber(GetCaiShenFangWeiFromDay(AYear, AMonth, ADay)));
  mmoResult.Lines.Add('喜神' + GetJiShenFangWeiFromNumber(GetXiShenFangWeiFromDay(AYear, AMonth, ADay)));
  mmoResult.Lines.Add('贵神' + GetJiShenFangWeiFromNumber(GetGuiShenFangWeiFromDay(AYear, AMonth, ADay)));
  mmoResult.Lines.Add('福神' + GetJiShenFangWeiFromNumber(GetFuShenFangWeiFromDay(AYear, AMonth, ADay)));

  mmoResult.Lines.Add('本日节气：' + GetJieQiFromNumber(GetJieQiFromDay(AYear, AMonth, ADay)));
  if GetJieQiFromDay(AYear, AMonth, ADay) >= 0 then
  begin
    if GetJieQiTimeFromDay(AYear, AMonth, ADay, H1, mi1) >= 0 then
      mmoResult.Lines.Add(Format('本日交节时刻：%d 时 %d 分', [H1, mi1])); 
  end;
  mmoResult.Lines.Add('每日胎神：' + GetTaiShenStringFromDay(AYear, AMonth, ADay));
  if Get3FuDay(AYear, AMonth, ADay, FSeq, FDay) then
    mmoResult.Lines.Add(Format('三伏： %s 第 %d 日', [Get3FuFromNumber(FSeq), FDay]));

  GetRuMeiDay(AYear, rmMonth, rmDay);
  GetChuMeiDay(AYear, cmMonth, cmDay);
  mmoResult.Lines.Add(Format('入梅 %d 月 %d 日，出梅 %d 月 %d 日', [rmMonth, rmDay, cmMonth, cmDay]));

  if GetLunarFromDay(AYear, AMonth, ADay, LunarYear, LunarMonth, LunarDay, IsLeap) then
  begin
    if IsLeap then
      mmoResult.Lines.Add(Format('农历 %d 年闰 %d 月 %s', [LunarYear, LunarMonth, GetLunarDayFromNumber(LunarDay)]))
    else
      mmoResult.Lines.Add(Format('农历 %d 年 %d 月 %s', [LunarYear, LunarMonth, GetLunarDayFromNumber(LunarDay)]));
  end;

  mmoResult.Lines.Add(Format('公历%d年各节气交接时刻：', [AYear]));
  for I := 0 to 11 do
  begin
    GetJieQiInAYear(AYear, 2 * I, M1, D1, H1, mi1);
    GetJieQiInAYear(AYear, 2 * I + 1, M2, D2, H2, mi2);
    mmoResult.Lines.Add(Format('%s：%2d月%2d日:%2d时:%2d分    %s：%2d月%2d日:%2d时:%2d分',
      [GetJieQiFromNumber((I * 2 + 22) mod 24), M1, D1, H1, mi1,
       GetJieQiFromNumber((I * 2 + 23) mod 24), M2, D2, H2, mi2]));
  end;
end;

procedure TFormCalendar.ConvertEditToDate;
begin
  AYear := StrToIntDef(edtYear.Text, 2000);
  AMonth := StrToIntDef(edtMonth.Text, 12);
  ADay := StrToIntDef(edtDay.Text, 21);
  AHour := StrToIntDef(edtHour.Text, 0);
end;

procedure TFormCalendar.FormCreate(Sender: TObject);
var
  Year, Month, Day: Word;
  Hour, Min, Sec, Dummy: Word;
begin
  DecodeDate(Date, Year, Month, Day);
  DecodeTime(Time, Hour, Min, Sec, Dummy);
  edtYear.Text := IntToStr(Year);
  edtMonth.Text := IntToStr(Month);
  edtDay.Text := IntToStr(Day);
  edtHour.Text := IntToStr(Hour);

  chkMonthButton.Checked := CnMonthCalendar1.ShowMonthButton;
  chkYearButton.Checked := CnMonthCalendar1.ShowYearButton;
  CnMonthCalendar1.Date := Now;
  pgc1.ActivePageIndex := 0;
end;

procedure TFormCalendar.Button1Click(Sender: TObject);
var
  HourObj: TCnHourObj;
begin
  ConvertEditToDate;
  mmoResult.Clear;

  HourObj := TCnHourObj.Create;
  try
    HourObj.Year := AYear;
    HourObj.Month := AMonth;
    HourObj.Day := ADay;
    HourObj.Hour := AHour;

    mmoResult.Lines.Add('星期：' + IntToStr(HourObj.Week));
    mmoResult.Lines.Add('年：'+ GetGanZhiFromNumber(HourObj.YearGanZhi));
    mmoResult.Lines.Add('月：'+ GetGanZhiFromNumber(HourObj.MonthGanZhi));
    mmoResult.Lines.Add('日：'+ GetGanZhiFromNumber(HourObj.DayGanZhi));
    mmoResult.Lines.Add('时：'+ GetGanZhiFromNumber(HourObj.HourGanZhi));
    mmoResult.Lines.Add('二十八宿：'+Get28XiuFromNumber(HourObj.Day28Xiu));

    if HourObj.IsLeapMonth then
      mmoResult.Lines.Add(Format('农历闰 %d 月 %d 日', [HourObj.LunarMonth, HourObj.LunarDay]))
    else
        mmoResult.Lines.Add(Format('农历 %d 月 %d 日', [HourObj.LunarMonth, HourObj.LunarDay]));

    if HourObj.IsInJiu then
      mmoResult.Lines.Add(Format('%d九第%d天', [HourObj.Jiu, HourObj.JiuDay]));
    if HourObj.IsInFu then
      mmoResult.Lines.Add(Format('%d伏第%d天', [HourObj.Fu, HourObj.FuDay]));
    mmoResult.Lines.Add('本日节气：' + GetJieQiFromNumber(HourObj.JieQi));
    mmoResult.Lines.Add(Format('入梅 %d 月 %d 日，出梅 %d 月 %d 日',
      [HourObj.RuMeiMonth, HourObj.RuMeiDay, HourObj.ChuMeiMonth, HourObj.ChuMeiDay]));

    mmoResult.Lines.Add('星座：' + GetXingZuoFromNumber(HourObj.XingZuo));
  finally
    HourObj.Free;
  end;
end;

procedure TFormCalendar.chkGanZhiClick(Sender: TObject);
begin
  CnMonthCalendar1.ShowGanZhi := chkGanZhi.Checked;
end;

procedure TFormCalendar.btnCalcLunarClick(Sender: TObject);
var
  Year, Month, Day: Integer;
  IsLeap: Boolean;
begin
//  for I := 620606 to 620608 do
//  begin
//    if GetDayFromEquStandardDays(I, Year, Month, Day) then
//    mmoLunar.Lines.Add(Format('%4.4d-%2.2d-%2.2d', [Year, Month, Day]));
//  end;

  Year := StrToIntDef(edtLunarYear.Text, 2000);
  Month := StrToIntDef(edtLunarMonth.Text, 12);
  Day := StrToIntDef(edtLunarDay.Text, 21);

  ValidLunarDate(Year, Month, Day, chkLeap.Checked);

  if not GetDayFromLunar(Year, Month, Day, chkLeap.Checked, Year, Month, Day) then
  begin
    mmoLunar.Lines.Add('No this Lunar Date.');
    Exit;
  end;

  mmoLunar.Lines.Add(Format('%4.4d-%2.2d-%2.2d', [Year, Month, Day]));
  if GetLunarFromDay(Year, Month, Day, Year, Month, Day, IsLeap) then
  begin
    if IsLeap then
      mmoLunar.Lines.Add(Format('农历 %d 年闰 %d 月 %s', [Year, Month, GetLunarDayFromNumber(Day)]))
    else
      mmoLunar.Lines.Add(Format('农历 %d 年 %d 月 %s', [Year, Month, GetLunarDayFromNumber(Day)]));
  end;

  mmoLunar.Lines.Add(Format('本农历月天数：%d', [GetLunarMonthDays(Year, Month, chkLeap.Checked)]));
end;

procedure TFormCalendar.chkMonthButtonClick(Sender: TObject);
begin
  Self.CnMonthCalendar1.ShowMonthButton := chkMonthButton.Checked;
end;

procedure TFormCalendar.chkYearButtonClick(Sender: TObject);
begin
  CnMonthCalendar1.ShowYearButton := chkYearButton.Checked;
end;

procedure TFormCalendar.dtpSetChange(Sender: TObject);
begin
  CnMonthCalendar1.Date := dtpSet.Date;
end;

procedure TFormCalendar.CnMonthCalendar1Change(Sender: TObject);
begin
  lblDate.Caption := DateTimeToStr(CnMonthCalendar1.Date);
end;

procedure TFormCalendar.btnSunTimeClick(Sender: TObject);
var
  Dt: TDateTime;
  J, W: Extended;
  T1, T2, T3: TDateTime;
begin
  Dt := EncodeDate(StrToIntDef(edtSunYear.Text, 2025), StrToIntDef(edtSunMonth.Text, 10),
    StrToIntDef(edtSunDay.Text, 1));
  J := StrToFloat(edtSunLongi.Text);
  W := StrToFloat(edtSunLat.Text);

  if GetSunRiseSetTime(Dt, J, W, 8, T1, T2, T3) = stNormal then
  begin
    mmoSun.Lines.Add('日出：' + TimeToStr(T1));
    mmoSun.Lines.Add('日中：' + TimeToStr(T2));
    mmoSun.Lines.Add('日落：' + TimeToStr(T3));
  end
  else
    mmoSun.Lines.Add('极昼或极夜');
end;

procedure TFormCalendar.btnSunAngleClick(Sender: TObject);
var
  Dt: TDateTime;
  J, W: Extended;
  A1, A2: Extended;
begin
  Dt := EncodeDate(StrToIntDef(edtSunYear.Text, 2025), StrToIntDef(edtSunMonth.Text, 10),
    StrToIntDef(edtSunDay.Text, 1));
  J := StrToFloat(edtSunLongi.Text);
  W := StrToFloat(edtSunLat.Text);

//  if GetSunRiseSetAzimuth(Dt, J, W, A1, A2) then
//  begin
//    mmoSun.Lines.Add('日出：' + FloatToStr(A1));
//    mmoSun.Lines.Add('日落：' + FloatToStr(A2));
//  end
//  else
//    mmoSun.Lines.Add('极昼或极夜');
end;

procedure TFormCalendar.btnEquStandardDaysClick(Sender: TObject);
var
  Day, Day1: Integer;
  Y, M, D:Integer;
begin
  mmoDays.Lines.Clear;
  for Day := 0 to 1577736 do
  begin
    GetDayFromEquStandardDays(Day, Y, M, D);
    Day1 := GetEquStandardDays(Y, M, D);
    if Day1 <> Day then
     mmoDays.Lines.Add('Error ' + IntToStr(Day1));
  end;

  if mmoDays.Lines.Count = 0 then
    ShowMessage('OK');
end;

procedure TFormCalendar.btnJulianDaysClick(Sender: TObject);
var
  I: Integer;
  J: Extended;
  Y, M, D: Integer;
begin
  mmoDays.Lines.Clear;

  for I := -2400000 to 610000 do
  begin
    if GetDayFromModifiedJulianDate(I + 0.5, Y, M, D) then
    begin
      J := GetModifiedJulianDate(Y, M, D);
      if J <> I + 0.5 then
      begin
        ShowMessage(IntToStr(I) + ' <> ' + FloatToStr(J));
        Exit;;
      end;
    end
    else
    begin
      ShowMessage('Error ' + IntToStr(I));
      Exit;
    end;
  end;

  if mmoDays.Lines.Count = 0 then
    ShowMessage('OK');
end;

procedure TFormCalendar.btnCheckDaysClick(Sender: TObject);
var
  Y, M, D: Integer;
  AY, AM, AD: Integer;
  JD, JD1: Extended;
begin
  Y := -4713;
  M := 1;
  D := 1;

  JD := GetJulianDate(Y, M, D);
  repeat
    StepToNextDay(Y, M, D);
    JD1 := GetJulianDate(Y, M, D);
    if JD1 - JD <> 1 then
    begin
      ShowMessage(Format('NOT ++ %d %d %d', [Y, M, D]));
      Exit;
    end;
    GetDayFromJulianDate(JD1, AY, AM, AD);
    if (AY <> Y) or (AM <> M) or (AD <> D) then
    begin
      ShowMessage(Format('NOT = %d %d %d', [Y, M, D]));
      Exit;
    end;
    JD := JD1;
  until Y > 3000;
  ShowMessage('Julian Date OK');

  Y := -4713;
  M := 1;
  D := 1;

  JD := GetModifiedJulianDate(Y, M, D);
  repeat
    StepToNextDay(Y, M, D);
    JD1 := GetModifiedJulianDate(Y, M, D);
    if JD1 - JD <> 1 then
    begin
      ShowMessage(Format('NOT ++ %d %d %d', [Y, M, D]));
      Exit;
    end;
    GetDayFromModifiedJulianDate(JD1, AY, AM, AD);
    if (AY <> Y) or (AM <> M) or (AD <> D) then
    begin
      ShowMessage(Format('NOT = %d %d %d', [Y, M, D]));
      Exit;
    end;
    JD := JD1;
  until Y > 3000;
  ShowMessage('Modified Julian Date OK');
end;

procedure TFormCalendar.btnEquStandardDays1Click(Sender: TObject);
var
  Y, M, D: Integer;
  AY, AM, AD: Integer;
  ED, ED1: Integer;
begin
  mmoDays.Lines.Clear;

  Y := -4713;  // 100, 200, 300, 500, 600, 700, 900, 1000, 1100, 1300, 1400, 1500
  M := 1;
  D := 1;

  ED := GetEquStandardDays(Y, M, D);
  repeat
    StepToNextDay(Y, M, D, True);  // 必须加 True 因为 GetEquStandardDays 只支持连续 0 年
    ED1 := GetEquStandardDays(Y, M, D);
    if ED1 - ED <> 1 then
    begin
      ShowMessage(Format('NOT ++ %d %d %d', [Y, M, D]));
      Exit;
    end;
    if GetDayFromEquStandardDays(ED1, AY, AM, AD) then
    begin
      if (AY <> Y) or (AM <> M) or (AD <> D) then
      begin
        mmoDays.Lines.Add(Format('NOT = %d %d %d %d', [Y, M, D, ED1]));
      end;
    end;
    ED := ED1;
  until Y > 3000;
  ShowMessage('EquStandardDay OK');
end;

procedure TFormCalendar.btnCheckLunarClick(Sender: TObject);
var
  Y, M, D: Integer;
  LY, LM, LD: Integer;
  Leap: Boolean;
begin
  mmoDays.Lines.Clear;

  Y := 1799;
  M := 12;
  D := 31;

  repeat
    StepToNextDay(Y, M, D);
    if GetLunarFromDay(Y, M, D, LY, LM, LD, Leap) then
    begin
      if LD = 1 then
      begin
        if Leap then
          mmoDays.Lines.Add(Format('公历 %4.4d %2.2d %2.2d -> 农历 %4.4d 闰%2.2d %2.2d',
            [Y, M, D, LY, LM, LD]))
        else
          mmoDays.Lines.Add(Format('公历 %4.4d %2.2d %2.2d -> 农历 %4.4d %2.2d %2.2d',
            [Y, M, D, LY, LM, LD]));
      end;
    end;
  until Y > 2100;
end;

const
  SCnLeapNumber: array[0..3648] of Integer = (
    0, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 8, 8, 8,
    9, 9, 9, 10, 10, 10, 11, 11, 12, 12, 12, 13, 13, 13, 14, 14, 15, 15,
    15, 16, 16, 16, 17, 17, 17, 18, 18, 19, 19, 19, 20, 20, 20, 21, 21,
    22, 22, 22, 23, 23, 23, 24, 24, 24, 25, 25, 26, 26, 26, 27, 27, 27,
    28, 28, 29, 29, 29, 30, 30, 30, 31, 31, 31, 32, 32, 33, 33, 33, 34,
    34, 34, 35, 35, 36, 36, 36, 37, 37, 37, 38, 38, 38, 39, 39, 40, 40,
    40, 41, 41, 41, 42, 42, 43, 43, 43, 44, 44, 44, 45, 45, 46, 46, 46,
    47, 47, 47, 48, 48, 48, 49, 49, 50, 50, 50, 51, 51, 52, 52, 52, 53,
    53, 53, 54, 54, 54, 55, 55, 56, 56, 56, 56, 57, 57, 57, 58, 58, 59,
    59, 59, 59, 60, 60, 61, 61, 62, 62, 63, 63, 64, 64, 64, 64, 65, 65,
    65, 65, 66, 66, 66, 67, 67, 68, 68, 69, 69, 69, 69, 70, 71, 71, 71,
    71, 71, 71, 72, 72, 73, 73, 74, 74, 74, 75, 75, 75, 75, 76, 76, 77,
    77, 77, 77, 78, 79, 79, 79, 79, 79, 80, 80, 80, 81, 82, 82, 82, 83,
    83, 84, 84, 84, 85, 85, 85, 86, 86, 86, 86, 87, 87, 87, 87, 88, 88,
    89, 89, 90, 90, 91, 91, 91, 92, 92, 93, 93, 94, 94, 94, 94, 95, 95,
    96, 96, 96, 96, 97, 97, 98, 98, 98, 99, 99, 100, 100, 100, 101, 101,
    101, 102, 102, 102, 103, 103, 104, 104, 104, 105, 105, 105, 106, 106,
    106, 107, 107, 107, 108, 108, 109, 109, 109, 110, 110, 111, 111, 111,
    112, 112, 112, 113, 113, 114, 114, 114, 115, 115, 116, 116, 116, 117,
    117, 117, 117, 118, 118, 119, 119, 119, 120, 120, 121, 121, 121, 122,
    122, 122, 123, 123, 124, 124, 124, 124, 125, 125, 126, 126, 126, 126,
    127, 127, 128, 128, 129, 129, 130, 130, 130, 130, 131, 131, 132, 132,
    132, 133, 133, 133, 134, 134, 135, 135, 135, 136, 136, 136, 137, 137,
    137, 138, 138, 139, 139, 139, 140, 140, 141, 141, 141, 142, 142, 142,
    143, 143, 143, 144, 144, 144, 145, 145, 146, 146, 146, 147, 147, 147,
    148, 148, 149, 149, 149, 150, 150, 150, 151, 151, 151, 152, 152, 153,
    153, 153, 154, 154, 154, 155, 155, 156, 156, 156, 157, 157, 157, 158,
    158, 158, 159, 159, 160, 160, 160, 161, 161, 161, 162, 162, 163, 163,
    163, 164, 164, 164, 165, 165, 165, 166, 166, 167, 167, 167, 168, 168,
    168, 169, 169, 170, 170, 170, 171, 171, 171, 172, 172, 172, 173, 173,
    174, 174, 174, 175, 175, 175, 176, 176, 177, 177, 177, 178, 178, 178,
    179, 179, 179, 180, 180, 181, 181, 181, 182, 182, 182, 183, 183, 184,
    184, 184, 185, 185, 185, 186, 186, 186, 187, 187, 188, 188, 188, 189,
    189, 189, 190, 190, 191, 191, 191, 192, 192, 192, 193, 193, 193, 194,
    194, 195, 195, 195, 196, 196, 196, 197, 197, 198, 198, 198, 199, 199,
    199, 200, 200, 200, 201, 201, 202, 202, 202, 203, 203, 203, 204, 204,
    205, 205, 205, 206, 206, 206, 207, 207, 207, 208, 208, 209, 209, 209,
    210, 210, 210, 211, 211, 212, 212, 212, 213, 213, 213, 214, 214, 214,
    214, 214, 215, 215, 215, 216, 216, 216, 217, 217, 218, 218, 218, 219,
    219, 219, 220, 220, 221, 221, 221, 222, 222, 222, 223, 223, 223, 224,
    224, 225, 225, 225, 226, 226, 226, 227, 227, 228, 228, 228, 229, 229,
    229, 230, 230, 230, 231, 231, 232, 232, 232, 233, 233, 233, 234, 234,
    235, 235, 235, 236, 236, 236, 237, 237, 237, 238, 238, 239, 239, 239,
    240, 240, 240, 241, 241, 242, 242, 242, 243, 243, 243, 244, 244, 244,
    245, 245, 246, 246, 246, 247, 247, 247, 248, 248, 249, 249, 249, 250,
    250, 250, 251, 251, 252, 252, 252, 253, 253, 253, 254, 254, 254, 255,
    255, 256, 256, 256, 257, 257, 257, 258, 258, 259, 259, 259, 260, 260,
    260, 261, 261, 261, 262, 262, 263, 263, 263, 264, 264, 264, 265, 265,
    266, 266, 266, 267, 267, 267, 268, 268, 268, 269, 269, 270, 270, 270,
    271, 271, 271, 272, 272, 273, 273, 273, 274, 274, 274, 275, 275, 276,
    276, 276, 277, 277, 277, 278, 278, 278, 279, 279, 280, 280, 280, 281,
    281, 281, 282, 282, 283, 283, 283, 284, 284, 284, 285, 285, 285, 286,
    286, 287, 287, 287, 288, 288, 288, 289, 289, 290, 290, 290, 291, 291,
    291, 292, 292, 292, 293, 293, 294, 294, 294, 295, 295, 295, 296, 296,
    297, 297, 297, 298, 298, 298, 299, 299, 299, 300, 300, 301, 301, 301,
    302, 302, 302, 303, 303, 304, 304, 304, 305, 305, 305, 306, 306, 306,
    307, 307, 308, 308, 308, 309, 309, 309, 310, 310, 311, 311, {左边是公元前 1 年}
    {右边是公元元年} 312, 312,
    312, 313, 313, 313, 314, 314, 315, 315, 315, 316, 316, 316, 317, 317,
    317, 318, 318, 319, 319, 319, 320, 320, 320, 321, 321, 322, 322, 322,
    323, 323, 323, 324, 324, 325, 325, 325, 326, 326, 326, 327, 327, 327,
    328, 328, 329, 329, 329, 330, 330, 330, 331, 331, 332, 332, 332, 333,
    333, 333, 334, 334, 334, 335, 335, 336, 336, 336, 337, 337, 337, 338,
    338, 339, 339, 339, 340, 340, 340, 341, 341, 341, 342, 342, 343, 343,
    343, 344, 344, 344, 345, 345, 346, 346, 346, 347, 347, 347, 348, 348,
    348, 349, 349, 350, 350, 350, 351, 351, 351, 352, 352, 353, 353, 353,
    354, 354, 354, 355, 355, 355, 356, 356, 357, 357, 357, 358, 358, 358,
    359, 359, 360, 360, 360, 361, 361, 361, 362, 362, 362, 363, 363, 364,
    364, 364, 365, 365, 365, 366, 366, 367, 367, 367, 368, 368, 368, 369,
    369, 369, 370, 370, 371, 371, 371, 372, 372, 372, 373, 373, 374, 374,
    374, 375, 375, 375, 376, 376, 376, 377, 377, 378, 378, 378, 379, 379,
    379, 380, 380, 381, 381, 381, 382, 382, 382, 383, 383, 383, 384, 384,
    385, 385, 385, 386, 386, 386, 387, 387, 388, 388, 388, 389, 389, 389,
    390, 390, 390, 391, 391, 392, 392, 392, 393, 393, 393, 394, 394, 395,
    395, 395, 396, 396, 396, 397, 397, 397, 398, 398, 399, 399, 399, 400,
    400, 400, 401, 401, 402, 402, 402, 403, 403, 403, 404, 404, 404, 405,
    405, 406, 406, 406, 407, 407, 407, 408, 408, 409, 409, 409, 410, 410,
    410, 411, 411, 411, 412, 412, 413, 413, 413, 414, 414, 414, 415, 415,
    416, 416, 416, 417, 417, 417, 418, 418, 418, 419, 419, 420, 420, 420,
    421, 421, 421, 422, 422, 423, 423, 423, 424, 424, 424, 425, 425, 425,
    426, 426, 427, 427, 427, 428, 428, 428, 429, 429, 430, 430, 430, 431,
    431, 431, 432, 432, 432, 433, 433, 434, 434, 434, 435, 435, 435, 436,
    436, 437, 437, 437, 438, 438, 438, 439, 439, 439, 440, 440, 441, 441,
    441, 442, 442, 442, 443, 443, 444, 444, 444, 445, 445, 445, 446, 446,
    446, 447, 447, 448, 448, 448, 449, 449, 449, 450, 450, 451, 451, 451,
    452, 452, 452, 453, 453, 453, 454, 454, 455, 455, 455, 456, 456, 456,
    457, 457, 458, 458, 458, 459, 459, 459, 460, 460, 460, 461, 461, 462,
    462, 462, 463, 463, 463, 464, 464, 465, 465, 465, 466, 466, 466, 467,
    467, 467, 468, 468, 469, 469, 469, 470, 470, 470, 471, 471, 472, 472,
    472, 473, 473, 473, 474, 474, 474, 475, 475, 475, 476, 476, 477, 477,
    477, 478, 478, 478, 479, 479, 480, 480, 480, 481, 481, 481, 482, 482,
    482, 483, 483, 484, 484, 484, 485, 485, 485, 486, 486, 487, 487, 487,
    488, 488, 488, 489, 489, 489, 490, 490, 491, 491, 491, 492, 492, 492,
    493, 493, 494, 494, 494, 495, 495, 495, 496, 496, 496, 497, 497, 498,
    498, 498, 499, 499, 499, 500, 500, 501, 501, 501, 502, 502, 502, 503,
    503, 503, 504, 504, 505, 505, 505, 506, 506, 506, 507, 507, 508, 508,
    508, 509, 509, 509, 510, 510, 510, 511, 511, 512, 512, 512, 513, 513,
    513, 514, 514, 515, 515, 515, 516, 516, 516, 517, 517, 517, 518, 518,
    519, 519, 519, 520, 520, 520, 521, 521, 522, 522, 522, 523, 523, 523,
    524, 524, 524, 525, 525, 526, 526, 526, 527, 527, 527, 528, 528, 529,
    529, 529, 530, 530, 530, 531, 531, 531, 532, 532, 533, 533, 533, 534,
    534, 534, 535, 535, 536, 536, 536, 537, 537, 537, 538, 538, 538, 539,
    539, 540, 540, 540, 541, 541, 541, 542, 542, 543, 543, 543, 544, 544,
    544, 545, 545, 545, 546, 546, 547, 547, 547, 548, 548, 548, 549, 549,
    550, 550, 550, 551, 551, 551, 552, 552, 552, 553, 553, 554, 554, 554,
    555, 555, 555, 556, 556, 557, 557, 557, 558, 558, 558, 559, 559, 559,
    560, 560, 561, 561, 561, 562, 562, 562, 563, 563, 563, 564, 564, 565,
    565, 565, 566, 566, 566, 567, 567, 568, 568, 568, 569, 569, 569, 570,
    570, 570, 571, 571, 572, 572, 572, 573, 573, 573, 574, 574, 575, 575,
    575, 576, 576, 576, 577, 577, 578, 578, 578, 579, 579, 579, 580, 580,
    580, 581, 581, 582, 582, 582, 583, 583, 583, 584, 584, 584, 585, 585,
    586, 586, 586, 587, 587, 587, 588, 588, 589, 589, 589, 590, 590, 590,
    591, 591, 591, 592, 592, 593, 593, 593, 594, 594, 594, 595, 595, 596,
    596, 596, 597, 597, 597, 598, 598, 598, 599, 599, 600, 600, 600, 601,
    601, 601, 602, 602, 603, 603, 603, 604, 604, 604, 605, 605, 605, 606,
    606, 607, 607, 607, 608, 608, 608, 609, 609, 610, 610, 610, 611, 611,
    611, 612, 612, 612, 613, 613, 614, 614, 614, 615, 615, 615, 616, 616,
    617, 617, 617, 618, 618, 618, 619, 619, 619, 620, 620, 621, 621, 621,
    622, 622, 622, 623, 623, 624, 624, 624, 625, 625, 625, 626, 626, 626,
    627, 627, 628, 628, 628, 629, 629, 629, 630, 630, 631, 631, 631, 632,
    632, 632, 633, 633, 633, 634, 634, 635, 635, 635, 636, 636, 636, 637,
    637, 638, 638, 638, 639, 639, 639, 640, 640, 640, 641, 641, 642, 642,
    642, 643, 643, 643, 644, 644, 645, 645, 645, 646, 646, 646, 647, 647,
    647, 648, 648, 649, 649, 649, 650, 650, 650, 651, 651, 652, 652, 652,
    653, 653, 653, 654, 654, 654, 655, 655, 656, 656, 656, 657, 657, 657,
    658, 658, 659, 659, 659, 660, 660, 660, 661, 661, 661, 662, 662, 663,
    663, 663, 664, 664, 664, 665, 665, 666, 666, 666, 667, 667, 667, 668,
    668, 668, 669, 669, 670, 670, 670, 671, 671, 671, 672, 672, 673, 673,
    673, 674, 674, 674, 675, 675, 675, 676, 676, 677, 677, 677, 678, 678,
    678, 679, 679, {公元 999 年与 1000 年分界} 680, {左边公元 1000 年，右边公元 1001 年}
    680, 680, 681, 681, 681, 682, 682, 682, 683, 683,
    684, 684, 684, 685, 685, 685, 686, 686, 687, 687, 687, 688, 688, 688,
    689, 689, 689, 690, 690, 691, 691, 691, 692, 692, 692, 693, 693, 694,
    694, 694, 695, 695, 695, 696, 696, 696, 697, 697, 698, 698, 698, 699,
    699, 699, 700, 700, 701, 701, 701, 702, 702, 702, 703, 703, 703, 704,
    704, 705, 705, 705, 706, 706, 706, 707, 707, 707, 708, 708, 709, 709,
    709, 710, 710, 710, 711, 711, 712, 712, 712, 713, 713, 713, 714, 714,
    714, 715, 715, 716, 716, 716, 717, 717, 717, 718, 718, 719, 719, 719,
    720, 720, 720, 721, 721, 721, 722, 722, 723, 723, 723, 724, 724, 724,
    725, 725, 726, 726, 726, 727, 727, 727, 728, 728, 728, 729, 729, 730,
    730, 730, 731, 731, 731, 732, 732, 733, 733, 733, 734, 734, 734, 735,
    735, 736, 736, 736, 737, 737, 737, 738, 738, 738, 739, 739, 740, 740,
    740, 741, 741, 741, 742, 742, 742, 743, 743, 744, 744, 744, 745, 745,
    745, 746, 746, 747, 747, 747, 748, 748, 748, 749, 749, 749, 750, 750,
    751, 751, 751, 752, 752, 752, 754, 754, 755, 755, 755, 756, 756, 756,
    757, 757, 757, 758, 758, 759, 759, 759, 760, 760, 760, 761, 761, 762,
    762, 762, 763, 763, 763, 764, 764, 764, 765, 765, 766, 766, 766, 767,
    767, 767, 768, 768, 769, 769, 769, 770, 770, 770, 771, 771, 771, 772,
    772, 773, 773, 773, 774, 774, 774, 775, 775, 776, 776, 776, 777, 777,
    777, 778, 778, 778, 779, 779, 780, 780, 780, 781, 781, 781, 782, 782,
    783, 783, 783, 784, 784, 784, 785, 785, 785, 786, 786, 787, 787, 787,
    788, 788, 788, 789, 789, 790, 790, 790, 791, 791, 791, 792, 792, 792,
    793, 793, 794, 794, 794, 795, 795, 795, 796, 796, 796, 797, 797, 798,
    798, 798, 799, 799, 799, 800, 800, 801, 801, 801, 802, 802, 802, 803,
    803, 804, 804, 804, 805, 805, 805, 806, 806, 806, 807, 807, 808, 808,
    808, 809, 809, 809, 810, 810, 810, 811, 811, 812, 812, 812, 813, 813,
    813, 814, 814, 815, 815, 815, 816, 816, 816, 817, 817, 818, 818, 818,
    819, 819, 819, 820, 820, 820, 821, 821, 822, 822, 822, 823, 823, 823,
    824, 824, 825, 825, 825, 826, 826, 826, 827, 827, 827, 828, 828, 829,
    829, 829, 830, 830, 830, 831, 831, 832, 832, 832, 833, 833, 833, 834,
    834, 834, 835, 835, 836, 836, 836, 837, 837, 837, 838, 838, 839, 839,
    839, 840, 840, 840, 841, 841, 841, 842, 842, 843, 843, 843, 844, 844,
    844, 845, 845, 845, 846, 846, 847, 847, 847, 848, 848, 848, 849, 849,
    850, 850, 850, 851, 851, 851, 852, 852, 852, 853, 853, 854, 854, 854,
    855, 855, 855, 856, 856, 857, 857, 857, 858, 858, 858, 859, 859, 859,
    860, 860, 861, 861, 861, 862, 862, 862, 863, 863, 864, 864, 863, 864,
    864, 864, 865, 865, 865, 866, 866, 867, 867, 867, 868, 868, 868, 869,
    869, 870, 870, 870, 871, 871, 871, 872, 872, 873, 873, 873, 874, 874,
    874, 875, 875, 875, 876, 876, 877, 877, 877, 878, 878, 878, 879, 879,
    879, 880, 880, 881, 881, 881, 882, 882, 882, 883, 883, 884, 884, 884,
    885, 885, 885, 886, 886, 886, 887, 887, 888, 888, 888, 889, 889, 889,
    890, 890, 891, 891, 891, 892, 892, 892, 893, 893, 893, 894, 894, 895,
    895, 895, 896, 896, 896, 897, 897, 898, 898, 898, 899, 899, 899, 900,
    900, 900, 901, 901, 902, 902, 902, 903, 903, 903, 904, 904, 905, 905,
    905, 906, 906, 906, 907, 907, 907, 908, 908, 909, 909, 909, 910, 910,
    910, 911, 911, 912, 912, 912, 913, 913, 913, 914, 914, 914, 915, 915,
    916, 916, 916, 917, 917, 917, 918, 918, 919, 919, 919, 920, 920, 920,
    921, 921, 921, 922, 922, 923, 923, 923, 924, 924, 924, 925, 925, 925,
    926, 926, 927, 927, 927, 928, 928, 928, 929, 929, 930, 930, 930, 931,
    931, 931, 932, 932, 932, 933, 933, 934, 934, 934, 935, 935, 935, 936,
    936, 937, 937, 937, 938, 938, 938, 939, 939, 939, 940, 940, 941, 941,
    941, 942, 942, 942, 943, 943, 944, 944, 944, 945, 945, 945, 946, 946,
    946, 947, 947, 948, 948, 948, 949, 949, 949, 950, 950, 951, 951, 951,
    952, 952, 952, 953, 953, 953, 954, 954, 955, 955, 955, 956, 956, 956,
    957, 957, 958, 958, 958, 959, 959, 959, 960, 960, 960, 961, 961, 962,
    962, 962, 963, 963, 963, 964, 964, 965, 965, 965, 966, 966, 966, 967,
    967, 967, 968, 968, 969, 969, 969, 970, 970, 970, 971, 971, 971, 972,
    972, 973, 973, 973, 974, 974, 974, 975, 975, 976, 976, 976, 977, 977,
    977, 978, 978, 978, 979, 979, 980, 980, 980, 981, 981, 981, 982, 982,
    983, 983, 983, 984, 984, 984, 985, 985, 986, 986, 986, 987, 987, 987,
    988, 988, 988, 989, 989, 990, 990, 990, 991, 991, 991, 992, 992, 993,
    993, 993, 994, 994, 994, 995, 995, 995, 996, 996, 997, 997, 997, 998,
    998, 998, 999, 999, 1000, 1000, 1000, 1001, 1001, 1001, 1002, 1002,
    1002, 1003, 1003, 1004, 1004, 1004, 1005, 1005, 1005, 1006, 1006,
    1006, 1007, 1007, 1008, 1008, 1008, 1009, 1009, 1009, 1010, 1010,
    1011, 1011, 1011, 1012, 1012, 1012, 1013, 1013, 1013, 1014, 1014,
    1015, 1015, 1015, 1016, 1016, 1016, 1017, 1017, 1018, 1018, 1018,
    1019, 1019, 1019, 1020, 1020, 1020, 1021, 1021, 1022, 1022, 1022,
    1023, 1023, 1023, 1024, 1024, 1025, 1025, 1025, 1026, 1026, 1026,
    1027, 1027, 1027, 1028, 1028, 1029, 1029, 1029, 1030, 1030, 1030,
    1031, 1031, 1032, 1032, 1032, 1033, 1033, 1033, 1034, 1034, 1034,
    1035, 1035, 1036, 1036, 1036, 1037, 1037, 1037, 1038, 1038, 1039,
    1039, 1039, 1040, 1040, 1040, 1041, 1041, 1042, 1042, 1042, 1043,
    1043, 1043, 1044, 1044, 1044, 1045, 1045, 1046, 1046, 1046, 1047,
    1047, 1047, {公元 1999 年与 2000 年分界} 1048, {左边公元 2000 年，右边公元 2001 年}
    1048, 1048, 1049, 1049, 1050, 1050, 1050, 1051,
    1051, 1051, 1052, 1052, 1053, 1053, 1053, 1054, 1054, 1054, 1055,
    1055, 1055, 1056, 1056, 1057, 1057, 1057, 1058, 1058, 1058, 1059,
    1059, 1060, 1060, 1060, 1061, 1061, 1061, 1062, 1062, 1062, 1063,
    1063, 1064, 1064, 1064, 1065, 1065, 1065, 1066, 1066, 1067, 1067,
    1067, 1068, 1068, 1068, 1069, 1069, 1069, 1070, 1070, 1071, 1071,
    1071, 1072, 1072, 1072, 1073, 1073, 1074, 1074, 1074, 1075, 1075,
    1075, 1076, 1076, 1076, 1077, 1077, 1078, 1078, 1078, 1079, 1079,
    1079, 1080, 1080, 1081, 1081, 1081, 1082, 1082, 1082, 1083, 1083,
    1083, 1084, 1084, {公元 2099 年与 2100 年分界线}
    1085, 1085, 1085, 1086, 1086, 1086, 1087, 1087, 1088, 1088, 1088,
    1089, 1089, 1089, 1090, 1090, 1090, 1091, 1091, 1092, 1092, 1092,
    1093, 1093, 1093, 1094, 1094, 1095, 1095, 1095, 1096, 1096, 1096,
    1097, 1097, 1097, 1098, 1098, 1099, 1099, 1099, 1100, 1100, 1100,
    1101, 1101, 1102, 1102, 1102, 1103, 1103, 1103, 1104, 1104, 1104,
    1105, 1105, 1106, 1106, 1106, 1107, 1107, 1107, 1108, 1108, 1109,
    1109, 1109, 1110, 1110, 1110, 1111, 1111, 1111, 1112, 1112, 1113,
    1113, 1113, 1114, 1114, 1114, 1115, 1115, 1115, 1116, 1116, 1117,
    1117, 1117, 1118, 1118, 1118, 1119, 1119, 1120, 1120, 1120, 1121,
    1121, 1121, 1122, 1122, 1123, 1123, 1123, 1124, 1124, 1124, 1125,
    1125, 1125, 1126, 1126, 1127, 1127, 1127, 1128, 1128, 1128, 1129,
    1129, 1130, 1130, 1130, 1131, 1131, 1131, 1132, 1132, 1132, 1133,
    1133, 1134, 1134, 1134, 1135, 1135, 1135, 1136, 1136, 1137, 1137,
    1137, 1138, 1138, 1138, 1139, 1139, 1139, 1140, 1140, 1141, 1141,
    1141, 1142, 1142, 1142, 1143, 1143, 1143, 1144, 1144, 1145, 1145,
    1145, 1146, 1146, 1146, 1147, 1147, 1148, 1148, 1148, 1149, 1149,
    1149, 1150, 1150, 1150, 1151, 1151, 1152, 1152, 1152, 1153, 1153,
    1153, 1154, 1154, 1155, 1155, 1155, 1156, 1156, 1156, 1157, 1157,
    1157, 1158, 1158, 1159, 1159, 1159, 1160, 1160, 1160, 1161, 1161,
    1162, 1162, 1162, 1163, 1163, 1163, 1164, 1164, 1165, 1165, 1165,
    1166, 1166, 1166, 1167, 1167, 1167, 1168, 1168, 1169, 1169, 1169,
    1170, 1170, 1170, 1171, 1171, 1171, 1172, 1172, 1173, 1173, 1173,
    1174, 1174, 1174, 1175, 1175, 1176, 1176, 1176, 1177, 1177, 1177,
    1178, 1178, 1178, 1179, 1179, 1180, 1180, 1180, 1181, 1181, 1181,
    1182, 1182, 1183, 1183, 1183, 1184, 1184, 1184, 1185, 1185, 1185,
    1186, 1186, 1187, 1187, 1187, 1188, 1188, 1188, 1189, 1189, 1190,
    1190, 1190, 1191, 1191, 1191, 1192, 1192, 1192, 1193, 1193, 1194,
    1194, 1194, 1195, 1195, 1195, 1196, 1196, 1197, 1197, 1197, 1198,
    1198, 1198, 1199, 1199, 1199, 1200, 1200, 1201, 1201, 1201, 1202,
    1202, 1202, 1203, 1203, 1204, 1204, 1204, 1205, 1205, 1205, 1206,
    1206, 1206, 1207, 1207, 1208, 1208, 1208, 1209, 1209, 1209, 1210,
    1210, 1211, 1211, 1211, 1212, 1212, 1212, 1213, 1213, 1213, 1214,
    1214, 1215, 1215, 1215, 1216, 1216, 1216, 1217, 1217, 1218, 1218,
    1218, 1219, 1219, 1219, 1220, 1220, 1220, 1221, 1221, 1222, 1222,
    1222, 1223, 1223, 1223, 1224, 1224, 1225, 1225, 1225, 1226, 1226,
    1226, 1227, 1227, 1227, 1228, 1228, 1229, 1229, 1229, 1230, 1230,
    1230, 1231, 1231, 1232, 1232, 1232, 1233, 1233, 1233, 1234, 1234,
    1234, 1235, 1235, 1236, 1236, 1236, 1237, 1237, 1237, 1238, 1238,
    1238, 1239, 1239, 1240, 1240, 1240, 1241, 1241, 1241, 1242, 1242,
    1243, 1243, 1243, 1244, 1244, 1244, 1245, 1245, 1245, 1246, 1246,
    1247, 1247, 1247, 1248, 1248, 1248, 1249, 1249, 1250, 1250, 1250,
    1251, 1251, 1251, 1252, 1252, 1253, 1253, 1253, 1254, 1254, 1254,
    1255, 1255, 1255, 1256, 1256, 1257, 1257, 1257, 1258, 1258, 1258,
    1259, 1259, 1260, 1260, 1260, 1261, 1261, 1261, 1262, 1262, 1262,
    1263, 1263, 1264, 1264, 1264, 1265, 1265, 1265, 1266, 1266, 1267,
    1267, 1267, 1268, 1268, 1268, 1269, 1269, 1269, 1270, 1270, 1271,
    1271, 1271, 1272, 1272, 1272, 1273, 1273, 1274, 1274, 1274, 1275,
    1275, 1275, 1276, 1276, 1276, 1277, 1277, 1278, 1278, 1278, 1279,
    1279, 1279, 1280, 1280, 1280, 1281, 1281, 1282, 1282, 1282, 1283,
    1283, 1283, 1284, 1284, 1285, 1285, 1285, 1286, 1286, 1286, 1287,
    1287, 1287, 1288, 1288, 1289, 1289, 1289, 1290, 1290, 1290, 1291,
    1291, 1292, 1292, 1292, 1293, 1293, 1293, 1294, 1294, 1294, 1295,
    1295, 1296, 1296, 1296, 1297, 1297, 1297, 1298, 1298, 1299, 1299,
    1299, 1300, 1300, 1300, 1301, 1301, 1301, 1302, 1302, 1303, 1303,
    1303, 1304, 1304, 1304, 1305, 1305, 1306, 1306, 1306, 1307, 1307,
    1307, 1308, 1308, 1308, 1309, 1309, 1310, 1310, 1310, 1311, 1311,
    1311, 1312, 1312, 1313, 1313, 1313, 1314, 1314, 1314, 1315, 1315,
    1316, 1316, 1316, 1317, 1317, 1317, 1318, 1318, 1318, 1319, 1319,
    1320, 1320, 1320, 1321, 1321, 1321, 1322, 1322, 1322, 1323, 1323,
    1324, 1324, 1324, 1325, 1325, 1325, 1326, 1326, 1327, 1327, 1327,
    1328, 1328, 1328, 1329, 1329, 1329, 1330, 1330, 1331, 1331, 1331,
    1332, 1332, 1332, 1333, 1333, 1334, 1334, 1334, 1335, 1335, 1335,
    1336, 1336, 1336, 1337, 1337, 1338, 1338, 1338, 1339, 1339, 1339,
    1340, 1340, 1341, 1341, 1341, 1342, 1342 {左边是公元 2799 年}
  );
  { * 自公元前 850 年开始的农历闰月数，-849~2100 移植自中国日历类，2100 后罗建仁计算补充
  0~3648 共 3649 项，包括公元前 850 年到公元前一年的 850 项及公元元年到公元 2799 年的 2799 项}

  SCnLeapMonth =
    '0c0080050010a0070030c0080050010a0070030c0080050020a0070030c0080050020a' +
    '0070030c0090050020a0070030c0090050020a0060030c0060030c00900600c0c0060c' +
    '00c00c00c0c000600c0c0006090303030006000c00c060c0006c00000c0c0c00600030' +
    '30006c00009009c0090c00c009000300030906030030c0c00060c00090c0060600c003' +
    '0060c00c003006009060030c0060060c0090900c00090c0090c00c0060300060600030' +
    '30c0c00030c0060030c0090060030c0090300c0080050020a0060030c0080050020b00' +
    '70030c0090050010a0070030b0090060020a0070040c0080050020a0060030c0080050' +
    '020b0070030c0090050010a0070030b0090060020a0070040c0080050020a0060030c0' +
    '080050020b0070030c0090050000c00900909009009090090090090900900909009009' +
    '0090900900909009009009090090090900900900909009009090090090900900900909' +
    '00900909009009009090090090900900900909009009090060030c0090050010a00700' +
    '30b008005001090070040c0080050020a0060030c0090040010a0060030c0090050010' +
    'a0070030b0080050010a008005001090050020a0060030c0080040010a0060030c0090' +   // 这里 70030b008005 中，b008 的第二个 0 是公元 0 年的非法数据！
    '050010a0070030b0080050010a0070030b008005001090070040c0080050020a006003' +
    '0c0080040010a0060030c0090050010a0070030b008005001090070040c0080050020a' +
    '0060030c0080040010a0060030c0090050010a0060030c0090050010a0070030b00800' +
    '5001090070040c0080050020a0060030c0080040010a0070030b0080050010a0070040' +
    'c0080050020a0060030c0080040010a0070030c0090050010a0070030b0080050020a0' +
    '060030c0080040010a0060030c0090050050020a0060030c0090050010b0070030c009' +
    '0050010a0070040c0080040020a0060030c0080050020a0060030c0090050010a00700' +
    '30b0080040020a0060040c0090050020b0070030c00a0050010a0070030b0090050020' +
    'a0070030c0080040020a0060030c0090050010a0070030c0090050030b007005001090' +
    '050020a007004001090060020c0070050c0090060030b0080040020a0060030b008004' +
    '0010a0060030b0080050010a0050040c0080050010a0060030c0080050010b0070030c' +
    '007005001090070030b0070040020a0060030c0080040020a0070030b0090050010a00' +
    '60040c0080050020a0060040c0080050010b0070030c007005001090070030c0080050' +
    '020a0070030c0090050020a0070030c0090050020a0060040c0090050020a0060040c0' +
    '090050010b0070030c0080050030b007004001090060020c008004002090060020a008' +
    '004001090050030b0080040020a0060040b0080040c00a0060020b0070050010900600' +
    '30b0070050020a0060020c008004002090070030c008005002090070040c0080040020' +
    'a0060040b0090050010a0060030b0080050020a0060040c0080050010b007003001080' +
    '05001090070030c0080050020a007003001090050030a0070030b0090050020a006004' +
    '0c0090050030b0070040c0090050010c0070040c0080060020b00700400a090060020b' +
    '007003002090060020a005004001090050030b007004001090050040c0080040c00a00' +
    '60020c007005001090060030b0070050020a0060020c008004002090060030b0080040' +
    '02090060030b0080040020a0060040b0080040010b0060030b0070050010a006004002' +
    '0700500308006004003070050030700600400307005003080060040030700500409006' +
    '0040030700500409006005002070050030a00600500307005004002060040020600500' +
    '30020600400307005004090060040030700500408007005003080050040a0060050030' +
    '7005004002060050030800500400206005002070050040020600500307006004002070' +
    '050030800600400307005004080060040a006005003080050040020700500409006004' +
    '002060050030b006005002070050030800600400307005004080060040030700500408' +
    '0060040020' +
    '700500409006004003070050040b006005002070050040b006005003070060040a0060' +
    '0500307006004002060050030700600409006004003070050040900700500308005004' +
    '0b00600500307006005001070050030800600400206005003070060040020600500307' +
    '0060040a00700500308006004003070050040800600500107005004080060050020700' +
    '50040a0060040020600500308006005002070050030800600400307005004080070050' +
    '030800500408006005003070050040a006005003070050040a00600500207005004001' +
    '0600500307006004001070050030700600408007005004070060040900600400307005'+
    '0040a007005003080060040b0060050030800600500107005003080060040020700500' +
    '3070060040030700500307006004003070050030800600400307005004090060050b00' +
    '7005004090060050020700600408006005003070060030800600500307006003080060'
    ;
  { * 自公元前 850 年开始的农历闰月信息 -849~2100，移植自中国日历类，2100 后罗建仁计算补充
  共 3650 项，竟然比上面的多一项，难道多写了个公元 0 年？}

procedure TFormCalendar.btnVerifyLeapNumberClick(Sender: TObject);
var
  I: Integer;
  Sum: Integer;
begin
  mmoDays.Lines.Clear;

  Sum := 0;
  // SCnLeapMonth[I] <-> SCnLeapNumber[I - 1];
  // I - 1 - 849 是公元年数，SCnLeapMonth 下标是 I，SCnLeapNumber 下标是 I - 1，且没有公元 0 年和 SCnLeapNumber 对应，所以公元后下标变成 I - 2

  for I := 1 to Length(SCnLeapMonth) do
  begin
    if SCnLeapMonth[I] <> '0' then
      Inc(Sum);

    if I - 1 - 849 = 0 then
    begin
      mmoDays.Lines.Add('Year: 0 Skip');
    end
    else if I - 1 - 849 < 0 then
    begin
      if SCnLeapMonth[I] <> '0' then
        mmoDays.Lines.Add(Format('Year: %d ... +%d %d %s', [I - 1 - 849, Sum, SCnLeapNumber[I - 1], SCnLeapMonth[I]]))
      else
        mmoDays.Lines.Add(Format('Year: %d ... +%d %d', [I - 1 - 849, Sum, SCnLeapNumber[I - 1]]));
    end
    else
    begin
      if SCnLeapMonth[I] <> '0' then
        mmoDays.Lines.Add(Format('Year: %d ... +%d %d %s', [I - 1 - 849, Sum, SCnLeapNumber[I - 2], SCnLeapMonth[I]]))
      else
        mmoDays.Lines.Add(Format('Year: %d ... +%d %d', [I - 1 - 849, Sum, SCnLeapNumber[I - 2]]));
    end;
  end;
end;

function NonZeroYearToZeroYear(AYear: Integer): Integer;
begin
  if AYear = 0 then
    raise ECnDateTimeException.Create('0 Year is Invalid');

  if AYear < 0 then
    Inc(AYear);
  Result := AYear;
end;

procedure TFormCalendar.btnCheckAllClick(Sender: TObject);
var
  Y, M, D: Integer;
  AD, ESD, AD0, ESD0: Integer;
  JD, MJD, JD0, MJD0: Extended;
begin
  mmoDays.Lines.Clear;

  Y := -4713;
  M := 1;
  D := 1;

  AD0 := GetAllDays(Y, M, D);
  ESD0 := GetEquStandardDays(NonZeroYearToZeroYear(Y), M, D);
  JD0 := GetJulianDate(Y, M, D);
  MJD0 := GetModifiedJulianDate(Y, M, D);

  repeat
    StepToNextDay(Y, M, D);

    AD := GetAllDays(Y, M, D);
    ESD := GetEquStandardDays(NonZeroYearToZeroYear(Y), M, D);
    JD := GetJulianDate(Y, M, D);
    MJD := GetModifiedJulianDate(Y, M, D);

    if (AD - AD0 <> 1) or (ESD - ESD0 <> 1) or (JD - JD0 <> 1) or (MJD - MJD0 <> 1) then
      mmoDays.Lines.Add(Format('Error %4.4d-%2.2d-%2.2d | %d>%d %d>%d %f>%f %f>%f',
        [Y, M, D, AD, AD0, ESD, ESD0, JD, JD0, MJD, MJD0]));

    AD0 := AD;
    ESD0 := ESD;
    JD0 := JD;
    MJD0 := MJD;
  until Y > 3000;

  if mmoDays.Lines.Count = 0 then
    ShowMessage('OK');
end;

end.
