{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2010 CnPack 开发组                       }
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

unit CnCalClass;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：历法计算类库
* 单元作者：刘啸 (liuxiao@cnpack.org)
* 备    注：
* 开发平台：PWinXP SP2 + Delphi 5
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2007.11.15 V1.2
*               增加二十八宿日的属性
*           2006.09.15 V1.0
*               增加农历日月的属性
*           2006.08.13 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

uses
  SysUtils, Classes, Windows, Math, CnCalendar;

type

 { Calendar Interfaces }

  ICnYearIntf = interface
    ['{7910FC7C-8B79-46B6-BAFE-558EE338FAB1}']
    function GetShengXiao: Integer;
    function GetYear: Integer;
    function GetYearGan: Integer;
    function GetYearZhi: Integer;
    function GetYearGanZhi: Integer;
    function GetRuMeiMonth: Integer;
    function GetRuMeiDay: Integer;
    function GetChuMeiMonth: Integer;
    function GetChuMeiDay: Integer;
    procedure SetYear(const Value: Integer);

    property Year: Integer read GetYear write SetYear;
    {* 公历年份 }
    property YearGan: Integer read GetYearGan;
    {* 年天干，0-9 对应 甲到癸 }
    property YearZhi: Integer read GetYearZhi;
    {* 年地支，0-11 对应 子到亥 }
    property YearGanZhi: Integer read GetYearGanZhi;
    {* 年天干地支，0-59 对应 甲子到癸亥 }
    property ShengXiao: Integer read GetShengXiao;
    {* 生肖，0-11 对应 鼠到猪 }
    property RuMeiMonth: Integer read GetRuMeiMonth;
    {* 入梅日的月份 }
    property RuMeiDay: Integer read GetRuMeiDay;
    {* 入梅日的日期 }
    property ChuMeiMonth: Integer read GetChuMeiMonth;
    {* 出梅日的月份 }
    property ChuMeiDay: Integer read GetChuMeiDay;
    {* 出梅日的日期 }
  end;

  ICnMonthIntf = interface(ICnYearIntf)
    ['{18547CBA-0751-4524-BC4B-FF733F10FAB2}']
    function GetMonth: Integer;
    function GetMonthGan: Integer;
    function GetMonthGanZhi: Integer;
    function GetMonthZhi: Integer;
    procedure SetMonth(const Value: Integer);

    procedure SetYearMonth(const AYear, AMonth: Integer);
    {* 一次性设置公历年月}
    property Month: Integer read GetMonth write SetMonth;
    {* 公历月份 }
    property MonthGan: Integer read GetMonthGan;
    {* 月天干，0-9 对应 甲到癸 }
    property MonthZhi: Integer read GetMonthZhi;
    {* 月地支，0-11 对应 子到亥 }
    property MonthGanZhi: Integer read GetMonthGanZhi;
    {* 月天干地支，0-59 对应 甲子到癸亥 }
  end;

  ICnDayIntf = interface(ICnMonthIntf)
    ['{85DBCE15-7281-4A58-BF19-5D29A5F318D6}']
    function GetDay: Integer;
    function GetWeek: Integer;
    function GetDayGan: Integer;
    function GetDayGanZhi: Integer;
    function GetDayZhi: Integer;
    function GetXingZuo: Integer;
    function GetDay28Xiu: Integer;
    function GetIsInJiu: Boolean;
    function GetJieQi: Integer;
    function GetJiu: Integer;
    function GetJiuDay: Integer;
    function GetIsInFu: Boolean;
    function GetFu: Integer;
    function GetFuDay: Integer;
    function GetLunarMonth: Integer;
    function GetLunarDay: Integer;
    function GetIsLeapMonth: Boolean;
    procedure SetDay(const Value: Integer);

    procedure SetYearMonthDay(const AYear, AMonth, ADay: Integer);
    {* 一次性设置公历年月日}
    property Day: Integer read GetDay write SetDay;
    {* 公历日期 }
    property Week: Integer read GetWeek;
    {* 星期，0-6 对应 日到六 }
    property DayGan: Integer read GetDayGan;
    {* 日天干，0-9 对应 甲到癸 }
    property DayZhi: Integer read GetDayZhi;
    {* 日地支，0-11 对应 子到亥 }
    property DayGanZhi: Integer read GetDayGanZhi;
    {* 日天干地支，0-59 对应 甲子到癸亥 }
    property XingZuo: Integer read GetXingZuo;
    {* 星座，0-11 对应 白羊到双鱼 }
    property Day28Xiu: Integer read GetDay28Xiu;
    {* 日二十八宿, 0-27 对应 角到轸}
    property JieQi: Integer read GetJieQi;
    {* 本日是本年的什么节气，0-23，对应立春到大寒，无则为 -1}
    property IsInJiu: Boolean read GetIsInJiu;
    {* 本日是否在数九日内 }
    property Jiu: Integer read GetJiu;
    {* 数九日中的第几九，1~9 对应一九到九九，不处于数九内则为 -1 }
    property JiuDay: Integer read GetJiuDay;
    {* 数九日中该九的第几日，1~9对应一九到九九，不处于数九内则为 -1 }
    property IsInFu: Boolean read GetIsInFu;
    {* 本日是否在三伏日内 }
    property Fu: Integer read GetFu;
    {* 三伏日中的第几伏，1~3 对应初伏到末伏，不处于三伏内则为 -1 }
    property FuDay: Integer read GetFuDay;
    {* 三伏日中伏中的第几日，1~10 对应伏内第一日到伏内第十日，不处于三伏内则为 -1 }
    property LunarMonth: Integer read GetLunarMonth;
    {* 农历月}
    property LunarDay: Integer read GetLunarDay;
    {* 农历日}
    property IsLeapMonth: Boolean read GetIsLeapMonth;
    {* 农历月是否闰月}
  end;

  ICnHourIntf = interface(ICnMonthIntf)
    ['{113BBE61-71B3-407A-948D-62699D15E2BA}']
    function GetHour: Integer;
    function GetHourGan: Integer;
    function GetHourZhi: Integer;
    function GetHourGanZhi: Integer;
    procedure SetHour(const Value: Integer);

    procedure SetYearMonthDayHour(const AYear, AMonth, ADay, AHour: Integer);
    {* 一次性设置公历年月日时}
    property Hour: Integer read GetHour write SetHour;
    {* 小时，24 时制 }
    property HourGan: Integer read GetHourGan;
    {* 时天干，0-9 对应 甲到癸 }
    property HourZhi: Integer read GetHourZhi;
    {* 时地支，也即时辰，0-11 对应 子到亥 }
    property HourGanZhi: Integer read GetHourGanZhi;
    {* 时天干地支，0-59 对应 甲子到癸亥 }
  end;

  { Calendar Classes }

  TCnYearObj = class(TInterfacedObject, ICnYearIntf)
  private
    FYear: Integer;
    FYearGan: Integer;
    FYearZhi: Integer;
    FRuMeiMonth: Integer;
    FRuMeiDay: Integer;
    FChuMeiMonth: Integer;
    FChuMeiDay: Integer;
  protected
    procedure Update; virtual;
  public
    constructor Create; virtual;

    function GetShengXiao: Integer;
    function GetYear: Integer;
    function GetYearGan: Integer;
    function GetYearZhi: Integer;
    function GetYearGanZhi: Integer;
    function GetRuMeiMonth: Integer;
    function GetRuMeiDay: Integer;
    function GetChuMeiMonth: Integer;
    function GetChuMeiDay: Integer;
    procedure SetYear(const Value: Integer);
  published
    property Year: Integer read GetYear write SetYear;
    property YearGan: Integer read GetYearGan;
    property YearZhi: Integer read GetYearZhi;
    property YearGanZhi: Integer read GetYearGanZhi;
    property ShengXiao: Integer read GetShengXiao;
    property RuMeiMonth: Integer read GetRuMeiMonth;
    property RuMeiDay: Integer read GetRuMeiDay;
    property ChuMeiMonth: Integer read GetChuMeiMonth;
    property ChuMeiDay: Integer read GetChuMeiDay;    
  end;

  TCnMonthObj = class(TCnYearObj, ICnMonthIntf)
  private
    FMonth: Integer;
    FMonthGan: Integer;
    FMonthZhi: Integer;
  protected
    procedure Update; override;
  public
    constructor Create; override;

    function GetMonth: Integer;
    function GetMonthGan: Integer;
    function GetMonthGanZhi: Integer;
    function GetMonthZhi: Integer;
    procedure SetMonth(const Value: Integer);
    procedure SetYearMonth(const AYear, AMonth: Integer);
  published
    property Month: Integer read GetMonth write SetMonth;
    property MonthGan: Integer read GetMonthGan;
    property MonthZhi: Integer read GetMonthZhi;
    property MonthGanZhi: Integer read GetMonthGanZhi;
  end;

  TCnDayObj = class(TCnMonthObj, ICnDayIntf)
  private
    FDay: Integer;
    FWeek: Integer;
    FDayGan: Integer;
    FDayZhi: Integer;
    FXingZuo: Integer;
    FDay28Xiu: Integer;
    FIsInJiu: Boolean;
    FJiu: Integer;
    FJiuDay: Integer;
    FIsInFu: Boolean;
    FFu: Integer;
    FFuDay: Integer;
    FLunarYear: Integer;
    FLunarMonth: Integer;
    FLunarDay: Integer;
    FIsLeapMonth: Boolean;
    function GetLunarYear: Integer;
  protected
    procedure Update; override;
  public
    constructor Create; override;

    function GetDay: Integer;
    function GetWeek: Integer;
    function GetDayGan: Integer;
    function GetDayGanZhi: Integer;
    function GetDayZhi: Integer;
    function GetXingZuo: Integer;
    function GetDay28Xiu: Integer;
    function GetJieQi: Integer;
    function GetIsInJiu: Boolean;
    function GetJiu: Integer;
    function GetJiuDay: Integer;
    function GetIsInFu: Boolean;
    function GetFu: Integer;
    function GetFuDay: Integer;
    function GetLunarMonth: Integer;
    function GetLunarDay: Integer;
    function GetIsLeapMonth: Boolean;
    procedure SetDay(const Value: Integer);
    procedure SetYearMonthDay(const AYear, AMonth, ADay: Integer);
  published
    property Day: Integer read GetDay write SetDay;
    property Week: Integer read GetWeek;
    property DayGan: Integer read GetDayGan;
    property DayZhi: Integer read GetDayZhi;
    property DayGanZhi: Integer read GetDayGanZhi;
    property XingZuo: Integer read GetXingZuo;
    property Day28Xiu: Integer read GetDay28Xiu;
    property JieQi: Integer read GetJieQi;
    property IsInJiu: Boolean read GetIsInJiu;
    property Jiu: Integer read GetJiu;
    property JiuDay: Integer read GetJiuDay;
    property IsInFu: Boolean read GetIsInFu;
    property Fu: Integer read GetFu;
    property FuDay: Integer read GetFuDay;
    property LunarYear: Integer read GetLunarYear;
    property LunarMonth: Integer read GetLunarMonth;
    property LunarDay: Integer read GetLunarDay;
    property IsLeapMonth: Boolean read GetIsLeapMonth;
  end;

  TCnHourObj = class(TCnDayObj, ICnHourIntf)
  private
    FHour: Integer;
    FHourGan: Integer;
    FHourZhi: Integer;
  protected
    procedure Update; override;    
  public
    constructor Create; override;

    function GetHour: Integer;
    function GetHourGan: Integer;
    function GetHourZhi: Integer;
    function GetHourGanZhi: Integer;
    procedure SetHour(const Value: Integer);
    procedure SetYearMonthDayHour(const AYear, AMonth, ADay, AHour: Integer);    
  published
    property Hour: Integer read GetHour write SetHour;
    property HourGan: Integer read GetHourGan;
    property HourZhi: Integer read GetHourZhi;
    property HourGanZhi: Integer read GetHourGanZhi;
  end;


implementation

{ TCnYearObj }

constructor TCnYearObj.Create;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(Now, AYear, AMonth, ADay);
  Year := AYear;
end;

function TCnYearObj.GetChuMeiDay: Integer;
begin
  Result := FChuMeiDay;
end;

function TCnYearObj.GetChuMeiMonth: Integer;
begin
  Result := FChuMeiMonth;
end;

function TCnYearObj.GetRuMeiDay: Integer;
begin
  Result := FRuMeiDay;
end;

function TCnYearObj.GetRuMeiMonth: Integer;
begin
  Result := FRuMeiMonth;
end;

function TCnYearObj.GetShengXiao: Integer;
begin
  Result := FYearZhi;
end;

function TCnYearObj.GetYear: Integer;
begin
  Result := FYear;
end;

function TCnYearObj.GetYearGan: Integer;
begin
  Result := FYearGan;
end;

function TCnYearObj.GetYearGanZhi: Integer;
begin
  Result := CombineGanZhi(FYearGan, FYearZhi);;
end;

function TCnYearObj.GetYearZhi: Integer;
begin
  Result := FYearZhi;
end;

procedure TCnYearObj.SetYear(const Value: Integer);
begin
  ValidDate(Value, 1, 1);
  FYear := Value;
  Update;
end;

procedure TCnYearObj.Update;
begin
  ExtractGanZhi(GetGanZhiFromYear(FYear), FYearGan, FYearZhi);
  CnCalendar.GetRuMeiDay(FYear, FRuMeiMonth, FRuMeiDay);
  CnCalendar.GetChuMeiDay(FYear, FChuMeiMonth, FChuMeiDay);
end;

{ TCnMonthObj }

constructor TCnMonthObj.Create;
var
  AYear, AMonth, ADay: Word;
begin
  inherited;
  DecodeDate(Now, AYear, AMonth, ADay);
  Month := AMonth;
end;

function TCnMonthObj.GetMonth: Integer;
begin
  Result := FMonth;
end;

function TCnMonthObj.GetMonthGan: Integer;
begin
  Result := FMonthGan;
end;

function TCnMonthObj.GetMonthGanZhi: Integer;
begin
  Result := CombineGanZhi(FMonthGan, FMonthZhi);
end;

function TCnMonthObj.GetMonthZhi: Integer;
begin
  Result := FMonthZhi;
end;

procedure TCnMonthObj.SetMonth(const Value: Integer);
begin
  ValidDate(Year, Value, 1);
  FMonth := Value;
  Update;
end;

procedure TCnMonthObj.SetYearMonth(const AYear, AMonth: Integer);
begin
  ValidDate(AYear, AMonth, 1);
  FYear := AYear;
  FMonth := AMonth;
  Update;
end;

procedure TCnMonthObj.Update;
begin
  inherited;
  // MonthObj 无日期信息，暂以 15 号为准来获取干支，以力求达到本月内。
  ExtractGanZhi(GetGanZhiFromMonth(Year, Month, 15), FMonthGan, FMonthZhi);
end;

{ TCnDayObj }

constructor TCnDayObj.Create;
var
  AYear, AMonth, ADay: Word;
begin
  inherited;
  DecodeDate(Now, AYear, AMonth, ADay);
  Day := ADay;
end;

function TCnDayObj.GetDay: Integer;
begin
  Result := FDay;
end;

function TCnDayObj.GetDay28Xiu: Integer;
begin
  Result := FDay28Xiu;
end;

function TCnDayObj.GetDayGan: Integer;
begin
  Result := FDayGan;
end;

function TCnDayObj.GetDayGanZhi: Integer;
begin
  Result := CombineGanZhi(FDayGan, FDayZhi);
end;

function TCnDayObj.GetDayZhi: Integer;
begin
  Result := FDayZhi;
end;

function TCnDayObj.GetFu: Integer;
begin
  Result := FFu;
end;

function TCnDayObj.GetFuDay: Integer;
begin
  Result := FFuDay;
end;

function TCnDayObj.GetIsInFu: Boolean;
begin
  Result := FIsInFu;
end;

function TCnDayObj.GetIsInJiu: Boolean;
begin
  Result := FIsInJiu;
end;

function TCnDayObj.GetIsLeapMonth: Boolean;
begin
  Result := FIsLeapMonth;
end;

function TCnDayObj.GetJieQi: Integer;
begin
  Result := GetJieQiFromDay(Year, Month, Day);
end;

function TCnDayObj.GetJiu: Integer;
begin
  Result := FJiu;
end;

function TCnDayObj.GetJiuDay: Integer;
begin
  Result := FJiuDay;
end;

function TCnDayObj.GetLunarDay: Integer;
begin
  Result := FLunarDay;
end;

function TCnDayObj.GetLunarMonth: Integer;
begin
  Result := FLunarMonth;
end;

function TCnDayObj.GetLunarYear: Integer;
begin
  Result := FLunarYear;
end;

function TCnDayObj.GetWeek: Integer;
begin
  Result := FWeek;
end;

function TCnDayObj.GetXingZuo: Integer;
begin
  Result := FXingZuo;
end;

procedure TCnDayObj.SetDay(const Value: Integer);
begin
  ValidDate(Year, Month, Value);
  FDay := Value;
  Update;
end;

procedure TCnDayObj.SetYearMonthDay(const AYear, AMonth, ADay: Integer);
begin
  ValidDate(AYear, AMonth, ADay);
  FYear := AYear;
  FMonth := AMonth;
  FDay := ADay;
  Update;
end;

procedure TCnDayObj.Update;
begin
  inherited;
  FWeek := CnCalendar.GetWeek(Year, Month, Day);
  // 有日期，重新以立春为界计算一下年月干支
  ExtractGanZhi(GetGanZhiFromYear(Year, Month, Day), FYearGan, FYearZhi);
  ExtractGanZhi(GetGanZhiFromMonth(Year, Month, Day), FMonthGan, FMonthZhi);

  ExtractGanZhi(GetGanZhiFromDay(Year, Month, Day), FDayGan, FDayZhi);
  FXingZuo := GetXingZuoFromMonthDay(Month, Day);
  FDay28Xiu := Get28XiuFromDay(Year, Month, Day);
  FIsInJiu := GetShu9Day(Year, Month, Day, FJiu, FJiuDay);
  FIsInFu := Get3FuDay(Year, Month, Day, FFu, FFuDay);

  GetLunarFromDay(Year, Month, Day, FLunarYear, FLunarMonth, FLunarDay, FIsLeapMonth);
end;

{ TCnHourObj }

constructor TCnHourObj.Create;
var
  AHour, AMin, ASec, AMSec: Word;
begin
  inherited;
  DecodeTime(Now, AHour, AMin, ASec, AMSec);
  Hour := AHour;
end;

function TCnHourObj.GetHour: Integer;
begin
  Result := FHour;
end;

function TCnHourObj.GetHourGan: Integer;
begin
  Result := FHourGan;
end;

function TCnHourObj.GetHourGanZhi: Integer;
begin
  Result := CombineGanZhi(FHourGan, FHourZhi);
end;

function TCnHourObj.GetHourZhi: Integer;
begin
  Result := FHourZhi;
end;

procedure TCnHourObj.SetHour(const Value: Integer);
begin
  ValidTime(Value, 1, 1);
  FHour := Value;
  Update;
end;

procedure TCnHourObj.SetYearMonthDayHour(const AYear, AMonth, ADay, AHour: Integer);
begin
  ValidDate(AYear, AMonth, ADay);
  ValidTime(AHour, 1, 1);
  FYear := AYear;
  FMonth := AMonth;
  FDay := ADay;
  FHour := AHour;  
  Update;
end;

procedure TCnHourObj.Update;
begin
  inherited;
  ExtractGanZhi(GetGanZhiFromHour(Year, Month, Day, Hour), FHourGan, FHourZhi);
end;

end.
