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

{*******************************************************************************
      中国日历类原始版权声明（本单元引用了其农历部分并改写成了 Pascal 代码）
  _______________________________________________________________________

  中国日历类（Chinese Calendar Class (CCC)）
  版本：v0.1，JavaScript版本

  版权所有 (C) 2002-2003 neweroica (wy25@mail.bnu.edu.cn)

  联系方式： Email:  wy25@mail.bnu.edu.cn

             QQ: 32460746
  ________________________________________________________________________

*******************************************************************************}

{*******************************************************************************
  寿星天文历原始版权声明（本单元引用了其精确节气计算部分并改写成了 Pascal 代码）
  _______________________________________________________________________

  本程序是开源的，你可以使用其中的任意部分代码，但不得随意修改“天文算法(eph.js)”
  及“农历算法(lunar.js)中古历部分的数据及算法”。一旦修改可能影响万年历的准确性，
  如果你对天文学不太了解而仅凭对历法的热情，请不要对此做任何修改，以免弄巧成拙。

  如果在你自己开发的软件中使用了本程序的核心算法及数据，你可以在你的软件中申明
  “数据或算法来源于寿星天文历”，也可以不申明，但不可以申明为它其它来源。
  如有异义，可与我共内探讨。

  作者：许剑伟，福建莆田第十中学。xunmeng04@163.com，13850262218
  ________________________________________________________________________

*******************************************************************************}

unit CnCalendar;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：中国历法计算实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
*           zjy (zjy@cnpack.org)
*           罗建仁
* 备    注：本单元实现了包括中国传统黄历在内的历法计算。包括星期、年月日时干支、年生肖、节气日期、星座、
*           阴阳五行、十二建（神）、三元、九运、九星、二十八宿、六曜、九九、三伏、吉神方位、十二及六十太岁等，
*           也包括公历、农历的互相转换。
*
*           注意，本单元中的公元前的公历年份除特殊说明外，均是正值直接变负值，比如公元前 1 年
*           便是 -1 年，没有公元 0 年，和部分函数及天文领域使用 0 作为公元前 1 年不同。
*
*           因天文计算精度及历史状况复杂的原因，农历在公元 250 年之前的准确度无法
*           确保与历史实际情况一致，使用时应注意。
*
*           公农历转换目前置闰的闰月是预置数据方式，不直接依赖于节气计算，
*           因而节气算法优化至精确度更高的方式，也即寿星天文历中的精确到秒的算法后，
*           不影响农历大小月判断与修正，避免了精度优化的过程中需要重新核对历史农历的繁文缛节。
*           但根据节气分隔年月日的干支计算会受到影响。
*
*           另外，目前精确的定气节气算法往前推至历史上，可能和历史上使用的平气节气有日期差异，
*           导致根据节气分隔年月日的干支计算也会出现偏差，使用时应注意。
*
* 开发平台：PWinXP SP2 + Delphi 2006
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2025.12.03 V3.0
*               涉及节气判断的场合全面改用精确节气算法，精确节气计算范围扩展至公元前
*           2025.08.14 V2.9
*               增加针对农历月、日的农历二十八宿算法，日不连续且无牛
*           2025.05.27 V2.8
*               增加并切换至移植自寿星天文历的精确节气算法，基本测试验证通过
*           2025.05.21 V2.7
*               增加儒略日/约化儒略日与公历年月日的互转及公历日步进函数
*               并修正等效标准日数在公元前可能有计算偏差的问题
*           2025.03.28 V2.6
*               增加干支纪年的年月转换，增加六曜日的计算，增加六十太岁字符串
*           2025.02.20 V2.5
*               根据清风徐来的报告，修正 2025 年农历 3 月的日期偏差问题
*           2022.09.03 V2.4
*               根据罗建仁的报告与查证，修正月干支在小寒节气前后可能有误的问题
*           2022.07.03 V2.3
*               根据罗建仁的报告与查证，修正 1582 年及之前节气有十天偏差的问题
*           2022.01.29 V2.2
*               根据日干增加每日吉神方位的计算，包括财神、喜神、福神、贵神等，其中贵神包括阳贵阴贵，默认阳贵
*           2018.08.22 V2.1
*               罗建仁补充 2100 年到 2800 年的农历数据并协助修正三伏日计算的偏差
*           2018.07.18 V2.0
*               根据通书算法更新九星的计算，增加节气至后甲子间的重排
*           2016.10.25 V1.9
*               加入九星的计算，包括年三元、年的运九星、年月日时九星
*           2012.02.24 V1.8
*               增加一精确到小时的年干支计算接口
*           2011.01.05 V1.7
*               月份的节气分界精确到分钟
*           2011.01.05 V1.7
*               加入一新方法，日干支计算加入小时参数以实现 23 时后是次日的机制
*           2010.04.12 V1.6
*               加入纳音五行长字符串的计算
*           2009.07.27 V1.5
*               修正一处计算农历日期时可能陷入死循环的问题
*           2009.07.16 V1.4
*               修正一处伏日计算不正确的问题，增加伏日字符串
*           2008.04.16 V1.3
*               增加干支阴阳、纳音、五行、十二建的计算与农历日期字符串的转换
*           2007.11.15 V1.2
*               增加二十八宿日的计算
*           2006.09.15 V1.1
*               增加公历到农历的部分计算，移植自中国日历类
*           2005.12.18 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Math;

const
  SCnYinYangArray: array[0..1] of string =
    ('阴', '阳');
  {* 阴阳字符串}

  SCnTianGanArray: array[0..9] of string =
    ('甲', '乙', '丙', '丁', '戊', '己', '庚', '辛', '壬', '癸');
  {* 天干字符串，Heavenly Stems}

  SCnDiZhiArray: array[0..11] of string =
    ('子', '丑', '寅', '卯', '辰', '巳', '午', '未', '申', '酉', '戌', '亥');
  {* 地支字符串，Earthly Branches}

  SCnShengXiaoArray: array[0..11] of string =
    ('鼠', '牛', '虎', '兔', '龙', '蛇', '马', '羊', '猴', '鸡', '狗', '猪');
  {* 生肖字符串，Zodiac Animals}

  SCnXingZuoArray: array[0..11] of string =
    ('白羊', '金牛', '双子', '巨蟹', '狮子', '处女',
     '天秤', '天蝎', '射手', '摩羯', '宝瓶', '双鱼');
  {* 星座字符串，Zodiac}

  SCn28XiuArray: array[0..27] of string =
    ('角', '亢', '氐', '房', '心', '尾', '箕',  // 东方青龙七宿，0-6
     '斗', '牛', '女', '虚', '危', '室', '壁',  // 北方玄武七宿，7-13
     '奎', '娄', '胃', '昴', '毕', '觜', '参',  // 西方白虎七宿，14-20
     '井', '鬼', '柳', '星', '张', '翼', '轸'); // 南方朱雀七宿，21-27
  {* 二十八宿字符串}

  SCn28XiuLongArray: array[0..27] of string =
    ('角木蛟', '亢金龙', '氐土貉', '房日兔', '心月狐', '尾火虎', '箕水豹',  // 东方青龙七宿，0-6
     '斗木獬', '牛金牛', '女土蝠', '虚日鼠', '危月燕', '室火猪', '壁水獝',  // 北方玄武七宿，7-13
     '奎木狼', '娄金狗', '胃土雉', '昴日鸡', '毕月乌', '觜火猴', '参水猿',  // 西方白虎七宿，14-20
     '井木犴', '鬼金羊', '柳土獐', '星日马', '张月鹿', '翼火蛇', '轸水蚓'); // 南方朱雀七宿，21-27
  {* 二十八宿完整名称字符串}

  SCnLunarMonthLeapName: string = '闰';
  SCnLunarMonthName: string = '月';
  SCnLunarMonthNameArray: array[0..11] of string =
    ('一', '二', '三', '四', '五', '六', '七', '八', '九', '十', '十一', '十二');
  {* 农历月份字符串}

  SCnLunarNumber1Array: array[0..10] of string =
    ('一', '二', '三', '四', '五', '六', '七', '八', '九', '十', '');
  {* 农历日期个位字符串}

  SCnLunarNumber2Array: array[0..5] of string =
    ('初', '十', '廿', '卅', '二', '三');
  {* 农历日期十位字符串}

  SCnWeekNumberArray: array[0..6] of string =
    ('日', '一', '二', '三', '四', '五', '六');
  {* 星期字符串}

  SCn5XingArray: array[0..4] of string =
    ('金', '木', '水', '火', '土');
  {* 五行字符串，以通常的金木水火土为顺序}

  SCn12JianArray: array[0..11] of string =
    ('建', '除', '满', '平', '定', '执', '破', '危', '成', '收', '开', '闭');
  {* 十二建字符串}

  SCn3FuArray: array[0..2] of string =
    ('初伏', '中伏', '末伏');
  {* 三伏字符串}

  SCnJieQiArray: array[0..23] of string = (
    '立春', // 节气  Beginning of Spring      3
    '雨水', // 中气  Rain Water               4
    '惊蛰', // 节气  Waking of Insects        5
    '春分', // 中气  Spring Equinox           6
    '清明', // 节气  Pure Brightness          7
    '谷雨', // 中气  Grain Rain               8
    '立夏', // 节气  Beginning of Summer      9
    '小满', // 中气  Lesser Fullness of Grain 10
    '芒种', // 节气  Grain in Beard           11
    '夏至', // 中气  Summer Solstice          12
    '小暑', // 节气  Lesser Heat              13
    '大暑', // 中气  Greater Heat             14
    '立秋', // 节气  Beginning of Autumn      15
    '处暑', // 中气  End of Heat              16
    '白露', // 节气  White Dew                17
    '秋分', // 中气  Autumn Equinox           18
    '寒露', // 节气  Cold Dew                 19
    '霜降', // 中气  Frost's Descent          20
    '立冬', // 节气  Beginning of Winter      21
    '小雪', // 中气  Lesser Snow              22
    '大雪', // 节气  Greater Snow             23
    '冬至', // 中气  Winter Solstice          24
    '小寒', // 节气  Lesser Cold              1，这是一公历年中的第一个节气
    '大寒'  // 中气  Greater Cold             2
  );
  {* 节气字符串，Solar Terms}

  SCn3YuanArray: array[0..2] of string =
    ( '上元', '中元', '下元' );
  {* 三元名称字符串}

  SCn9XingArray: array[0..8] of string =
    ( '一白', '二黑', '三碧', '四绿', '五黄', '六白', '七赤', '八白', '九紫');
  {* 九星名称字符串}

  SCn9Xing5XingArray: array[0..8] of string =
    ( '水', '土', '木', '木', '土', '金', '金', '土', '火');
  {* 九星所属五行名称字符串}

  SCn9XingStarArray: array[0..8] of string =
    ( '贪狼', '巨门', '禄存', '文曲', '廉贞', '武曲', '破军', '左辅', '右弼');
  {* 九星的星宿名称字符串}

  SCn6YaoArray: array[0..5] of string =
    ('先胜', '友引', '先负', '佛灭', '大安', '赤口');
  {* 六曜日的名称字符串}

  SCnTaiShen1Array: array[0..59] of string =
    ( '占门碓', '碓磨厕', '厨灶炉', '仓库门', '房床厕',
      '占门床', '占碓磨', '厨灶厕', '仓库炉', '房床门',

      '门鸡栖', '碓磨床', '厨灶碓', '仓库厕', '房床炉',
      '占大门', '碓磨栖', '厨灶床', '仓库碓', '房床厕',

      '占门炉', '碓磨门', '厨灶栖', '仓库碓', '房床碓',
      '占门厕', '碓磨炉', '厨灶炉', '仓库栖', '占房床',

      '占门碓', '碓磨厕', '厨灶炉', '仓库门', '房床栖',
      '占门床', '占碓磨', '厨灶厕', '仓库卢', '房床门',

      '门鸡栖', '碓磨床', '厨灶碓', '仓库厕', '仓库厕',
      '占大门', '碓磨栖', '厨灶床', '仓库碓', '房床厕',

      '占门炉', '碓磨门', '厨灶栖', '仓库床', '房床碓',
      '占门厕', '碓磨炉', '厨灶门', '仓库栖', '占门床' );
  {* 每日胎神位置字符串，与六十干支轮排对应}

  SCnTaiShen2Array: array[0..59] of string =
    ( '外东南', '外东南', '外正南', '外正南', '外正南',
      '外正南', '外正南', '外西南', '外西南', '外西南',

      '外西南', '外西南', '在西南', '外正西', '外正西',
      '外正西', '外正西', '外正西', '外西北', '外西北',

      '外西北', '外西北', '外西北', '外正北', '外正北',
      '外正北', '外正北', '外正北', '外正北', '房内北',

      '房内北', '房内北', '房内北', '房内北', '房内南',
      '房内南', '房内南', '房内南', '房内南', '房内南',

      '房内东', '房内东', '房内东', '房内东', '房内东',
      '外东北', '外东北', '外东北', '外东北', '外东北',

      '外东北', '外正东', '外正东', '外正东', '外正东',
      '外正东', '外东南', '外东南', '外东南', '外东南' );
  {* 每日胎神方位字符串，与六十干支轮排对应}

  SCnNaYinWuXingArray: array[0..29] of string =
    ( '海中金', '炉中火', '大林木',
      '路旁土', '剑锋金', '山头火',

      '涧下水', '城墙土', '白蜡金',
      '杨柳木', '泉中水', '屋上土',

      '霹雷火', '松柏木', '长流水',
      '沙中金', '山下火', '平地木',

      '壁上土', '金箔金', '佛灯火',
      '天河水', '大驿土', '钗钏金',

      '桑柘木', '大溪水', '沙中土',
      '天上火', '石榴木', '大海水' );
  {* 纳音五行字符串，与相邻一对六十干支对应}

  SCnJiShenFangWeiArray: array[0..7] of string =
    ( '正北', '东北', '正东', '东南',
      '正南', '西南', '正西', '西北');
  {* 吉神方位字符串，对应八卦的八个方向。
     吉神包括喜神、财神、贵神，贵神还包括阴贵、阳贵，默认指阳贵}

  SCnGanZhiArray: array[0..59] of string =
    ( '甲子', '乙丑', '丙寅', '丁卯', '戊辰', '己巳', '庚午', '辛未', '壬申', '癸酉',   // 0-9
      '甲戌', '乙亥', '丙子', '丁丑', '戊寅', '己卯', '庚辰', '辛巳', '壬午', '癸未',   // 10-19
      '甲申', '乙酉', '丙戌', '丁亥', '戊子', '己丑', '庚寅', '辛卯', '壬辰', '癸巳',   // 20-29
      '甲午', '乙未', '丙申', '丁酉', '戊戌', '己亥', '庚子', '辛丑', '壬寅', '癸卯',   // 30-39
      '甲辰', '乙巳', '丙午', '丁未', '戊申', '己酉', '庚戌', '辛亥', '壬子', '癸丑',   // 40-49
      '甲寅', '乙卯', '丙辰', '丁巳', '戊午', '己未', '庚申', '辛酉', '壬戌', '癸亥' ); // 50-59
  {* 六十干支字符串，Sexagenary Cycle}

  SCn12TaiSuiArray: array[0..11] of string =
    ( '太岁', '太阳', '丧门', '太阴', '五鬼', '死符', '岁破', '龙德', '白虎', '福德', '天狗', '病符');
  {* 十二太岁名称字符串，与十二地支对应}

  SCn60TaiSuiArray: array[0..59] of string =
    ( '金辨', '陈材', '耿章', '沈兴', '赵达', '郭灿', '王济', '李素', '刘旺', '康志',   // 0-9
      '施广', '任保', '郭嘉', '汪文', '鲁先', '龙仲', '董德', '郑但', '陆明', '魏仁',   // 10-19
      '方杰', '蒋崇', '白敏', '封济', '邹铛', '傅佑', '邬桓', '范宁', '彭泰', '徐单',   // 20-29
      '章词', '杨仙', '管仲', '唐杰', '姜武', '谢太', '卢秘', '杨信', '贺谔', '皮时',   // 30-39
      '李诚', '吴遂', '文哲', '缪丙', '徐浩', '程宝', '倪秘', '叶坚', '丘德', '朱得',   // 40-49
      '张朝', '万清', '辛亚', '杨彦', '黎卿', '傅党', '毛梓', '石政', '洪充', '虞程' ); // 50-59
  {* 六十太岁名称字符串，与六十干支对应}

type
  ECnDateTimeException = class(Exception);
  {* 历法相关异常}

  TCnCalendarType = (ctinvalid, ctJulian, ctGregorian);
  {* 日历类型：      非法，     儒略，    格里高利}

  TCnLunarMonthType = (lmtSmall, lmtBig);
  {* 农历月类型：      小月，    大月}

  TCnEclipseType = (etNone, etSolar, etMoonFull, etMoonHalf);
  {* 日月食类型：   无，    日食，   月全食，    月偏食 }

  TCnMoonPhase = (mpNone, mpShuo, mpWang);
  {* 月相：       无，    朔，    望}

  TCnSunRiseSetType = (stNormal, stAllwaysUp, stAllwaysDown, stError);
  {* 日出日落类型：    普通，    极昼，       极夜，        数据错误 }

function GetSunRiseSetTime(ADate: TDateTime; Longitude, Latitude: Extended;
  ZoneTime: Integer; out RiseTime, TransitTime, SetTime: TDateTime):
  TCnSunRiseSetType;
{* 计算某经纬度地点在某公历日期的日出日落时刻。

   参数：
     ADate: TDateTime                     - 日期
     Longitude: Extended                  - 经度
     Latitude: Extended                   - 纬度
     ZoneTime: Integer                    - 该经度所在的时区，比如国内的经纬度应传 8
     out RiseTime: TDateTime              - 返回日出时间，如果无日出返回 -1
     out TransitTime: TDateTime           - 返回日中时间，如果无日中返回 -1
     out SetTime: TDateTime               - 返回日落时间，如果无日落返回 -1

   返回值：Boolean                        - 返回日出日落类型
}

function GetDateIsValid(AYear, AMonth, ADay: Integer): Boolean;
{* 返回公历日期是否合法。

   参数：
     AYear, AMonth, ADay: Integer         - 待判断的公历年、月、日

   返回值：Boolean                        - 返回是否合法
}

procedure ValidDate(AYear, AMonth, ADay: Integer);
{* 判断公历日期是否合法，不合法则抛出异常。

   参数：
     AYear, AMonth, ADay: Integer         - 待判断的公历年、月、日

   返回值：（无）
}

function GetLunarDateIsValid(ALunarYear, ALunarMonth, ALunarDay: Integer;
  IsLeapMonth: Boolean = False): Boolean;
{* 返回农历日期是否合法。

   参数：
     ALunarYear, ALunarMonth, ALunarDay: Integer          - 待判断的农历年、月、日
     IsLeapMonth: Boolean                                 - 该农历日期是否闰月

   返回值：Boolean                                        - 返回是否合法
}

procedure ValidLunarDate(ALunarYear, ALunarMonth, ALunarDay: Integer;
  IsLeapMonth: Boolean = False);
{* 判断农历日期是否合法，不合法则抛出异常。

   参数：
     ALunarYear, ALunarMonth, ALunarDay: Integer          - 待判断的农历年、月、日
     IsLeapMonth: Boolean                                 - 该农历日期是否闰月

   返回值：（无）
}

function GetTimeIsValid(AHour, AMinitue, ASecond: Integer): Boolean;
{* 返回时间是否合法。

   参数：
     AHour, AMinitue, ASecond: Integer    - 待判断的时、分、秒

   返回值：Boolean                        - 返回是否合法
}

procedure ValidTime(AHour, AMinitue, ASecond: Integer);
{* 判断时间是否合法，不合法则抛出异常。

   参数：
     AHour, AMinitue, ASecond: Integer    - 待判断的时、分、秒

   返回值：（无）
}

procedure StepToNextDay(var AYear, AMonth, ADay: Integer;
  ZeroYear: Boolean = False);
{* 公历年月日往后步进一天，考虑各种闰年、格里高利历删 10 天等因素。支持公历无 0 年
   （-1 是闰年）及公历有 0 年（0 年闰年）两种模式。默认不允许出现公历 0 年。

   参数：
     var AYear, AMonth, ADay: Integer     - 待步进的公历年、月、日
     ZeroYear: Boolean                    - 是否允许出现公元 0 年以便于特殊场合计算
}

function GetMonthDays(AYear, AMonth: Integer): Integer;
{* 取公历年的某月天数，不考虑 1582 年 10 月的特殊情况。

   参数：
     AYear, AMonth: Integer               - 某公历年及某个月，公历年不能为 0，如传 0 则二月当成平年

   返回值：Integer                        - 返回该月天数
}

function GetLunarMonthDays(ALunarYear, ALunarMonth: Integer;
  IsLeapMonth: Boolean = False): Integer;
{* 取农历年的某月天数。

   参数：
     ALunarYear, ALunarMonth: Integer     - 某农历年及某个月，农历年不能为 0
     IsLeapMonth: Boolean                 - 该农历月是否闰月

   返回值：Integer                        - 返回该月天数
}

function GetIsLeapYear(AYear: Integer): Boolean;
{* 返回某公历是否闰年，自动判断儒略历还是格里高利历法，支持公元前，
   公元前按儒略历的公元后正四倍数闰年往前连续推，譬如公元前 1 年、前 5 年……为闰年。

   参数：
     AYear: Integer                       - 待计算的公历年份，不能为 0

   返回值：Boolean                        - 返回是否闰年
}

function GetDayFromYearBegin(AYear, AMonth, ADay: Integer): Integer; overload;
{* 取某公历日期到年初的天数，不考虑 1582 年 10 月的特殊情况，年份值不能为 0。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回该日到年初的天数
}

function GetDayFromYearBegin(AYear, AMonth, ADay, AHour: Integer;
  AMinute: Integer = 0; ASecond: Integer = 0): Extended; overload;
{* 取某日期到年初的天数，小时、分、秒数折算入小数，不考虑 1582 年 10 月的特殊情况，年份值不能为 0。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日
     AHour, AMinute, ASecond: Integer     - 待计算的时、分、秒

   返回值：Extended                       - 返回该时刻到年初的天数，时间折算成小数
}

function ExtractMonthDay(Days: Integer; AYear: Integer; out AMonth: Integer;
  out ADay: Integer): Boolean;
{* 从距年初天数返回月和日数，年份用来判断是否是闰年，返回 False 表示不合法日期。

   参数：
     Days: Integer                        - 待计算的距年初天数
     AYear: Integer                       - 公历年，用来判断是否闰年
     out AMonth: Integer                  - 返回月
     out ADay: Integer                    - 返回日

   返回值：Boolean                        - 返回日期是否合法
}

function GetWeek(const AValue: TDateTime): Integer; overload;
{* 获得某公历日期是星期几，0-6 对应日到六。

   参数：
     const AValue: TDateTime              - 待计算的日期

   返回值：Integer                        - 返回星期几
}

function GetWeek(AYear, AMonth, ADay: Integer): Integer; overload;
{* 获得某公历日期是星期几，0-6 对应日到六。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回星期几
}

function GetWeekFromNumber(const AValue: Integer): string;
{* 从数字获得星期名，不包括“星期”二字, 0-6 对应日到六。

   参数：
     const AValue: Integer                - 待计算的星期数字

   返回值：string                         - 返回星期字符串
}

function GetYinYangFromNumber(const AValue: Integer): string;
{* 从数字获得阴阳名, 0-1 对应阴阳。

   参数：
     const AValue: Integer                - 待计算的阴阳数字

   返回值：string                         - 返回阴阳字符串
}

function Get5XingFromNumber(const AValue: Integer): string;
{* 从数字获得五行名, 0-4 对应金木水火土。

   参数：
     const AValue: Integer                - 待计算的五行数字

   返回值：string                         - 返回五行字符串
}

function Get12JianFromNumber(const AValue: Integer): string;
{* 从数字获得十二建名, 0-11 对应建除满平定执破危成收开闭。

   参数：
     const AValue: Integer                - 待计算的十二建数字

   返回值：string                         - 返回十二建字符串
}

function Get3FuFromNumber(const AValue: Integer): string;
{* 从数字获得三伏名, 0-2 对应初伏中伏末伏。

   参数：
     const AValue: Integer                - 待计算的三伏数字

   返回值：string                         - 返回三伏字符串
}

function GetTianGanFromNumber(const AValue: Integer): string;
{* 从数字获得天干名, 0-9 对应甲乙丙丁戊己庚辛壬癸。

   参数：
     const AValue: Integer                - 待计算的天干数字

   返回值：string                         - 返回天干字符串
}

function GetDiZhiFromNumber(const AValue: Integer): string;
{* 从数字获得地支名, 0-11 对应子丑寅卯辰巳午未申酉戌亥。

   参数：
     const AValue: Integer                - 待计算的地支数字

   返回值：string                         - 返回地支字符串
}

function GetGanZhiFromNumber(const AValue: Integer): string;
{* 从数字获得天干地支名, 0-59 对应的不一一列出了。

   参数：
     const AValue: Integer                - 待计算的干支数字

   返回值：string                         - 返回干支字符串
}

function Get12TaiSuiFromNumber(const AValue: Integer): string;
{* 从地支数字获得十二太岁名, 0-11 对应太岁到病符。

   参数：
     const AValue: Integer                - 待计算的地支数字

   返回值：string                         - 返回十二太岁字符串
}

function Get60TaiSuiFromNumber(const AValue: Integer): string;
{* 从干支数字获得六十太岁名, 0-59 对应的不一一列出了。

   参数：
     const AValue: Integer                - 待计算的干支数字

   返回值：string                         - 返回六十太岁字符串
}

function GetShengXiaoFromNumber(const AValue: Integer): string;
{* 从数字获得生肖名, 0-11 对应鼠牛虎兔龙蛇马羊猴鸡狗猪。

   参数：
     const AValue: Integer                - 待计算的生肖数字

   返回值：string                         - 返回生肖字符串
}

function GetJieQiFromNumber(const AValue: Integer): string;
{* 从数字获得节气名, 0-23 对应的不一一列出了。

   参数：
     const AValue: Integer                - 待计算的节气数字

   返回值：string                         - 返回节气字符串
}

function GetXingZuoFromNumber(const AValue: Integer): string;
{* 从数字获得星座名, 0-11 对应的不一一列出了。

   参数：
     const AValue: Integer                - 待计算的星座数字

   返回值：string                         - 返回星座字符串
}

function Get28XiuFromNumber(const AValue: Integer): string;
{* 从数字获得二十八宿名, 0-27 对应的不一一列出了。

   参数：
     const AValue: Integer                - 待计算的二十八宿数字

   返回值：string                         - 返回二十八宿名字符串
}

function Get28XiuLongFromNumber(const AValue: Integer): string;
{* 从数字获得二十八宿完整名, 0-27 对应的不一一列出了。

   参数：
     const AValue: Integer                - 待计算的二十八宿数字

   返回值：string                         - 返回二十八宿完整名字符串
}

function GetLunarMonthFromNumber(const AMonth: Integer; IsLeap: Boolean): string;
{* 从数字获得农历月名称, 1-12。

   参数：
     const AMonth: Integer                - 待计算的农历月份数
     IsLeap: Boolean                      - 是否闰月

   返回值：string                         - 返回农历月字符串
}

function GetLunarDayFromNumber(const ADay: Integer): string;
{* 从数字获得农历日名称, 1-30。

   参数：
     const ADay: Integer                  - 待计算的农历日数

   返回值：string                         - 返回农历日字符串
}

function GetYinYangFromGan(const Gan: Integer): Integer;
{* 从天干获得其阴阳, 0-9 转换成 0-1。

   参数：
     const Gan: Integer                   - 待计算的天干数

   返回值：Integer                        - 返回阴阳
}

function GetYinYangFromZhi(const Zhi: Integer): Integer;
{* 从地支获得其阴阳, 0-11 转换成 0-1。

   参数：
     const Zhi: Integer                   - 待计算的地支数

   返回值：Integer                        - 返回阴阳
}

function CombineGanZhi(Gan, Zhi: Integer): Integer;
{* 将天干地支组合成干支，0-9 0-11 转换成 0-59。注意是六十轮排，不是任意两个干支都能组合。

   参数：
     Gan, Zhi: Integer                    - 待组合的干数和支数

   返回值：Integer                        - 返回干支数，如果组合失败则返回 -1
}

function ExtractGanZhi(GanZhi: Integer; out Gan: Integer; out Zhi: Integer): Boolean;
{* 将干支拆分成天干地支，0-59 转换成 0-9 及 0-11。

   参数：
     GanZhi: Integer                      - 待拆分的干支数
     out Gan: Integer                     - 返回干数
     out Zhi: Integer                     - 返回支数

   返回值：Boolean                        - 返回是否拆分成功
}

function Get5XingFromGan(const Gan: Integer): Integer;
{* 获得某干的五行，0-4 对应金木水火土。

   参数：
     const Gan: Integer                   - 待计算的干数

   返回值：Integer                        - 返回五行
}

function Get5XingFromZhi(const Zhi: Integer): Integer;
{* 获得某支的五行，0-4 对应金木水火土。

   参数：
     const Zhi: Integer                   - 待计算的支数

   返回值：Integer                        - 返回五行
}

function Get5XingFromGanZhi(const GanZhi: Integer): Integer; overload;
{* 获得某干支的纳音五行（短），0-4 对应金木水火土。

   参数：
     const GanZhi: Integer                - 待计算的干支数

   返回值：Integer                        - 返回纳音五行
}

function Get5XingFromGanZhi(Gan, Zhi: Integer): Integer; overload;
{* 获得某干支的纳音五行（短），0-4 对应金木水火土

   参数：
     Gan, Zhi: Integer                    - 待计算的干数与支数

   返回值：Integer                        - 返回纳音五行
}

function Get5XingFromDay(AYear, AMonth, ADay: Integer): Integer;
{* 获得某公历日的纳音五行（短），0-4 对应金木水火土。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回纳音五行
}

function Get5XingLongFromGanZhi(const GanZhi: Integer): string; overload;
{* 获得某干支的纳音五行（长），返回字符串。

   参数：
     const GanZhi: Integer                - 待计算的干支数

   返回值：string                         - 返回纳音五行字符串
}

function Get5XingLongFromGanZhi(Gan, Zhi: Integer): string; overload;
{* 获得某干支的纳音五行（长），返回字符串。

   参数：
     Gan, Zhi: Integer                    - 待计算的干支数

   返回值：string                         - 返回纳音五行长字符串
}

function Get5XingLongFromDay(AYear, AMonth, ADay: Integer): string;
{* 获得某公历日的纳音五行（长），返回字符串。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：string                         - 返回纳音五行长字符串
}

function Get3HeFromZhi(const Zhi: Integer; out He1: Integer;
  out He2: Integer): Boolean;
{* 获得某地支的另外两个三合。

   参数：
     const Zhi: Integer                   - 待计算的地支数
     out He1: Integer                     - 返回三合地支之一
     out He2: Integer                     - 返回三合地支之二

   返回值：Boolean                        - 返回是否计算成功
}

function GetGanZhiFromHour(AYear, AMonth, ADay, AHour: Integer): Integer;
{* 获得某公历时的天干地支，0-59 对应 甲子到癸亥。

   参数：
     AYear, AMonth, ADay, AHour: Integer  - 待计算的公历年、月、日、时

   返回值：Integer                        - 返回时干支
}

function GetGanZhiFromDay(AYear, AMonth, ADay: Integer): Integer; overload;
{* 获得某公历日的天干地支，0-59 对应甲子到癸亥。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回日干支
}

function GetGanZhiFromDay(AYear, AMonth, ADay, AHour: Integer): Integer; overload;
{* 获得某公历日的天干地支，0-59 对应 甲子到癸亥，小时参数用于判断 23 小时后是次日。

   参数：
     AYear, AMonth, ADay, AHour: Integer  - 待计算的公历年、月、日、时

   返回值：Integer                        - 返回日干支
}

function GetGanZhiFromDay(AllDays: Integer): Integer; overload;
{* 获得形式为绝对天数的某公历日的天干地支，0-59 对应甲子到癸亥，参数为距离公元元年 1 月 0 日的绝对天数。

   参数：
     AllDays: Integer                     - 待计算的公历绝对天数

   返回值：Integer                        - 返回日干支
}

function GetGanZhiFromMonth(AYear, AMonth, ADay: Integer): Integer; overload;
{* 获得某公历月的天干地支，需要日是因为月以节气分界，不考虑时。0-59 对应甲子到癸亥。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回月干支
}

function GetGanZhiFromMonth(AYear, AMonth, ADay, AHour: Integer): Integer; overload;
{* 获得某公历月的天干地支，需要日与时是因为月以节气分界。0-59 对应甲子到癸亥。

   参数：
     AYear, AMonth, ADay, AHour: Integer  - 待计算的公历年、月、日、时

   返回值：Integer                        - 返回月干支
}

function GetGanZhiFromYear(AYear: Integer): Integer; overload;
{* 获得某公/农历年的天干地支，0-59 对应甲子到癸亥。

   参数：
     AYear: Integer                       - 待计算的公历年或农历年

   返回值：Integer                        - 返回年干支
}

function GetGanZhiFromYear(AYear, AMonth, ADay: Integer): Integer; overload;
{* 根据公历年月日获得某公历年的天干地支，以立春为年分界，0-59 对应甲子到癸亥。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回年干支
}

function GetGanZhiFromYear(AYear, AMonth, ADay, AHour: Integer): Integer; overload;
{* 根据公历年月日时获得某公历年的天干地支，以立春为年分界，精确到小时，0-59 对应甲子到癸亥。

   参数：
     AYear, AMonth, ADay, AHour: Integer  - 待计算的公历年、月、日、时

   返回值：Integer                        - 返回年干支
}

function GetGanFromYear(AYear: Integer): Integer;
{* 获得某公/农历年的天干，0-9 对应甲到癸。

   参数：
     AYear: Integer                       - 待计算的公历年或农历年

   返回值：Integer                        - 返回天干
}

function GetZhiFromYear(AYear: Integer): Integer;
{* 获得某公/农历年的地支，0-11 对应子到亥

   参数：
     AYear: Integer                       - 待计算的公历年或农历年

   返回值：Integer                        - 返回地支
}

function GetShengXiaoFromYear(AYear: Integer): Integer;
{* 获得某公/农历年的生肖也就是地支，0-11 对应鼠到猪。

   参数：
     AYear: Integer                       - 待计算的公历年

   返回值：Integer                        - 返回生肖
}

function GetXingZuoFromMonthDay(AMonth, ADay: Integer): Integer;
{* 获得某公历月日的星座，0-11 对应白羊到双鱼。

   参数：
     AMonth, ADay: Integer                - 待计算的公历月、日

   返回值：Integer                        - 返回星座
}

function Get12JianFromDay(AYear, AMonth, ADay: Integer): Integer;
{* 获得某公历月日的十二建，0-11 对应建到闭。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回十二建
}

function Get28XiuFromDay(AYear, AMonth, ADay: Integer): Integer;
{* 获得某公历日的二十八宿，0-27 对应角到轸。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回二十八宿
}

function GetLunar28XiuFromDay(AYear, AMonth, ADay: Integer): Integer;
{* 获得某公历日的农历二十八宿，0-27 对应角到轸，如农历日不存在或超界，返回 -1
   注意该农历二十八宿的结果中不包括 8 牛。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回农历二十八宿
}

function GetTaiShenStringFromDay(AYear, AMonth, ADay: Integer): string; overload;
{* 获得某公历日的胎神方位，0-59 返回胎神位置加胎神方位的字符串。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：string                         - 返回胎神方位字符串
}

function GetTaiShenStringFromDay(AYear, AMonth, ADay: Integer;
  out TaiShen1: string; out TaiShen2: string): Boolean; overload;
{* 获得某公历日的胎神方位，0-59 返回胎神位置与胎神方位两个字符串。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日
     out TaiShen1: string                 - 返回胎神位置字符串
     out TaiShen2: string                 - 返回胎神方位字符串

   返回值：Boolean                        - 返回是否获取成功
}

function GetShiChenFromHour(AHour: Integer): Integer;
{* 获得小时时刻对应的时辰，0-11 对应子至亥。

   参数：
     AHour: Integer                       - 待计算的小时数

   返回值：Integer                        - 返回时辰
}

function AdjustYearToGanZhi(var AYear: Integer; AMonth: Integer;
  ADay: Integer; AHour: Integer): Boolean;
{* 根据立春为界，调整公历年的年月日的年份数到标准干支纪年，供黄历中针对年的干支
   等概念的计算。注意按约定的规则，立春当天 0 时起就属于新年，哪怕立春交接时刻
   在 0 时后的某一时刻，月份分界的节气也类似。

   参数：
     var AYear: Integer                   - 供调整的公历年，调整后的结果也放其中
     AMonth: Integer                      - 该公历日期的月份数
     ADay: Integer                        - 该公历日期的日数
     AHour: Integer                       - 该公历日期的小时数

   返回值：Boolean                        - 返回日期是否合法，注意与是否调整无关
}

function AdjustYearMonthToGanZhi(var AYear: Integer; var AMonth: Integer;
  ADay: Integer; AHour: Integer): Boolean;
{* 根据立春与节气为界，调整公历年的年月日的年份数与月份数到标准干支纪年，
   供黄历中针对月的干支等概念的计算。注意按约定的规则，立春当天 0 时起就属于新年，
   哪怕立春交接时刻在 0 时后的某一时刻，月份分界的节气也类似。

   参数：
     var AYear: Integer                   - 供调整的公历年，调整后的结果也放其中
     var AMonth: Integer                  - 供调整的公历月，调整后的结果也放其中
     ADay: Integer                        - 该公历日期的日数
     AHour: Integer                       - 该公历日期的小时数

   返回值：Boolean                        - 返回日期是否合法，注意与是否调整无关
}

function Get3YuanFromNumber(A3Yuan: Integer): string;
{* 从数字获得三元名称，0-2。

   参数：
     A3Yuan: Integer                      - 待获取的三元数字

   返回值：string                         - 返回三元名称
}

function Get9XingFromNumber(A9Xing: Integer): string;
{* 从数字获得九星名称，0-8。

   参数：
     A9Xing: Integer                      - 待获取的九星数字

   返回值：string                         - 返回九星名称
}

function Get6YaoFromNumber(A6Yao: Integer): string;
{* 从数字获得六曜名称，0-5。

   参数：
     A6Yao: Integer                       - 待获取的六曜数字

   返回值：string                         - 返回六曜名称
}

function Get3YuanFromYear(AYear, AMonth, ADay: Integer): Integer;
{* 获取公历年所属的三元，0-2。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回三元
}

function GetYun9XingFromYear(AYear, AMonth, ADay: Integer): Integer;
{* 获取公历年的运九星，0-8 对应一白到九紫。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回运九星
}

function Get9XingFromYear(AYear, AMonth, ADay: Integer): Integer;
{* 获取公历年的年九星，0-8 对应一白到九紫。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回年九星
}

function Get9XingFromMonth(AYear, AMonth, ADay: Integer): Integer;
{* 获取公历月的月九星，0-8 对应一白到九紫。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回月九星
}

function Get9XingFromDay(AYear, AMonth, ADay: Integer): Integer;
{* 获取公历日的日九星，0-8 对应一白到九紫。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回日九星
}

function Get9XingFromHour(AYear, AMonth, ADay, AHour: Integer): Integer;
{* 获取公历时的时九星，0-8 对应一白到九紫。

   参数：
     AYear, AMonth, ADay, AHour: Integer  - 待计算的公历年、月、日、时

   返回值：Integer                        - 返回时九星
}

function Get6YaoFromDay(AYear, AMonth, ADay: Integer): Integer;
{* 获取公历日的日六曜，0-5 对应先胜到赤口。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回日六曜
}

function GetJiShenFangWeiFromNumber(AFangWei: Integer): string;
{* 根据吉神（包括财神、喜神、福神、贵神）方位数字获得吉神方位名称。

   参数：
     AFangWei: Integer                    - 待获取的吉神方位数

   返回值：string                         - 返回吉神方位名称
}

function GetCaiShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
{* 获得公历年月日的财神方位，0-7。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回财神方位
}

function GetXiShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
{* 获得公历年月日的喜神方位，0-7。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回喜神方位
}

function GetFuShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
{* 获得公历年月日的福神方位，0-7。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回福神方位
}

function GetGuiShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
{* 获得公历年月日的贵神方位，0-7，默认为阳贵。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回贵神方位，默认为阳贵
}

function GetYangGuiShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
{* 获得公历年月日的阳贵神方位，0-7。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回阳贵神方位
}

function GetYingShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
{* 获得公历年月日的阴贵神方位，0-7。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回阴贵神方位
}

function GetAllDays(AYear, AMonth, ADay: Integer): Integer;
{* 获得距公元元年 1 月 0 日的绝对天数。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回绝对天数
}

function GetJieQiInAYear(AYear, N: Integer; out AMonth: Integer; out ADay: Integer;
  out AHour: Integer; out AMinitue: Integer; out ASecond: Integer; out ActualYear: Integer): Boolean;
{* 获得某公历年内的第 N 个节气的交节月日时分秒，0~23，对应物理顺序的小寒到冬至，并非立春到大寒。
   注意：公历 1582 年以前的第 0 个节气小寒可能会落到前一年的 12 月底，本过程处理了这种情况。
   ActualYear 参数会返回本节气落在的实际年份。

   参数：
     AYear: Integer                       - 待计算的公历年
     N: Integer                           - 待计算的节气序号
     out AMonth: Integer                  - 返回节气所在月份
     out ADay: Integer                    - 返回节气所在日
     out AHour: Integer                   - 返回节气交接时刻的小时数
     out AMinitue: Integer                - 返回节气交接时刻的分钟数
     out ASecond: Integer                 - 返回节气交接时刻的秒数
     out ActualYear: Integer              - 返回本节气实际所在的年份，公元 1582 年以前的第 0 个节气小寒可能落到前一年

   返回值：Boolean                        - 返回计算是否成功
}

function GetJieQiFromDay(AYear, AMonth, ADay: Integer): Integer;
{* 获得公历年月日是本年的什么节气（或者次年的小寒），0-23，对应立春到大寒，无则返回 -1。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回节气序号，-1 为不是节气
}

function GetJieQiTimeFromDay(AYear, AMonth, ADay: Integer;
  out AHour: Integer; out AMinitue: Integer; out ASecond: Integer): Integer;
{* 获得公历年月日是本年的什么节气（或者次年的小寒）以及交节时刻，0-23，对应立春到大寒，无则返回 -1。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日
     out AHour: Integer                   - 返回节气交接时刻的小时数
     out AMinitue: Integer                - 返回节气交接时刻的分钟数
     out ASecond: Integer                 - 返回节气交接时刻的秒数

   返回值：Integer                        - 返回节气序号，-1 为不是节气
}

function GetJieQiDayTimeFromYear(AYear, N: Integer): Extended;
{* 获得某公历年内的第 N 个节气距年初的以天为单位的精确时间，1-24，对应小寒到冬至。
   支持公元前的负公历年，不能为 0。1582 年 10 月前有约十天偏差，导致小寒可能返回负值。
   注：底层函数，一般不直接调用。

   参数：
     AYear: Integer                       - 待计算的公历年
     N: Integer                           - 待计算的节气序数，1-24，对应小寒到冬至

   返回值：Extended                       - 返回该节气交接时刻距年初的以天为单位的精确时间
}

function GetShu9Day(AYear, AMonth, ADay: Integer; out JiuSeq: Integer; out JiuDay: Integer): Boolean;
{* 获得公历年月日在数九日中的第几九的第几日，1~9,1~9 对应一九到九九，返回 False 为不在数九日内。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日
     out JiuSeq: Integer                  - 返回数九的第几九
     out JiuDay: Integer                  - 返回本九内第几日

   返回值：Boolean                        - 返回是否在数九日内
}

function Get3FuDay(AYear, AMonth, ADay: Integer; out FuSeq: Integer; out FuDay: Integer): Boolean;
{* 获得公历年月日在三伏日中的第几伏的第几日，0~2,1~10（或 20）对应初伏到末伏的伏日，返回 False 为不在三伏日内。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日
     out FuSeq: Integer                   - 返回三伏的第几伏
     out FuDay: Integer                   - 返回本伏内第几日

   返回值：Boolean                        - 返回是否在三伏日内
}

function GetRuMeiDay(AYear: Integer; out AMonth: Integer; out ADay: Integer): Boolean;
{* 获得某公历年中的入梅日期，梅雨季节的开始日，芒种后的第一个丙日，返回是否获取成功。

   参数：
     AYear: Integer                       - 待计算的公历年
     out AMonth: Integer                  - 返回的入梅日期所在月份
     out ADay: Integer                    - 返回的入梅日

   返回值：Boolean                        - 返回是否获取成功
}

function GetChuMeiDay(AYear: Integer; out AMonth: Integer; out ADay: Integer): Boolean;
{* 获得某公历年中的出梅日期，梅雨季节的结束日，小暑后的第一个未日，返回是否获取成功。

   参数：
     AYear: Integer                       - 待计算的公历年
     out AMonth: Integer                  - 返回的出梅日期所在月份
     out ADay: Integer                    - 返回的出梅日

   返回值：Boolean                        - 返回是否获取成功
}

function GetLunarFromDay(AYear, AMonth, ADay: Integer;
  out LunarYear, LunarMonth, LunarDay: Integer; out IsLeapMonth: Boolean): Boolean;
{* 获得某公历年月日的农历年月日和是否闰月，公历年不能为 0，返回是否获取成功。
   注意因历史上的确出现过修历增补月份的，这里暂且用“闰月”标记其第二个月。
   比如：
   公元 239 年 12 月 13 日十二月大后，公元 240 年 1 月 12 日增加十二月小，本来不算闰月，
   但为了以示区分，240 年 1 月 12 日及以后的十二月返回 IsLeapMonth 为 True。
   公元 23 年 12 月 2 日十二月小后，12 月 31 日增加十二月大，也不叫闰月，
   同样为了以示区分，12 月 31 日及以后的十二月返回 IsLeapMonth 为 True。

   参数：
     AYear, AMonth, ADay: Integer                         - 待计算的公历年、月、日
     out LunarYear, LunarMonth, LunarDay: Integer         - 用来容纳计算结果的农历年、月、日
     out IsLeapMonth: Boolean                             - 返回是否闰月

   返回值：Boolean                                        - 返回是否计算成功
}

function GetLunarMonthDayFromDay(AYear, AMonth, ADay: Integer;
  out LunarMonth, LunarDay: Integer; out IsLeapMonth: Boolean): Boolean;
{* 获得某公历年月日的农历月日和是否闰月的信息，年份按相等处理，实际可能不等。返回是否获取成功。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日
     out LunarMonth, LunarDay: Integer    - 用来容纳计算结果的农历月、日
     out IsLeapMonth: Boolean             - 返回是否闰月

   返回值：Boolean                        - 返回是否计算成功
}

function GetLunarLeapMonth(AYear: Integer): Integer;
{* 获得某农历年的第几个月是闰月，返回 1~12 对应第一个月到第十二个月，
   也就是去年闰十二月到今年闰十一月，返回 0 表示无闰月。

   参数：
     AYear: Integer                       - 待计算的农历年，基本等同于公历年，无公元 0 年，如果传了 0 这不存在的年也返回无闰月

   返回值：Integer                        - 返回闰月，0 表示无闰月
}

function GetDayFromLunar(ALunarYear, ALunarMonth, ALunarDay: Integer; IsLeapMonth:
  Boolean; out AYear, AMonth, ADay: Integer): Boolean;
{* 获得某农历年月日（加是否闰月）的公历年月日，农历年不能为 0，返回是否获取成功。
   注意因历史上的确出现过修历增补月份的。比如：
   公元 239 年 12 月 13 日十二月大后，公元 240 年 1 月 12 日增加十二月小，又不算闰月，因而农历 239 年十二月内的就查不出来。
   公元 23 年 12 月 2 日十二月小后，12 月 31 日增加十二月大，也不叫闰月，因而农历 23 年十二月内的也查不出来。

   参数：
     ALunarYear, ALunarMonth, ALunarDay: Integer          - 待计算的农历年、月、日，农历年不能为 0
     IsLeapMonth: Boolean                                 - 是否闰月
     out AYear, AMonth, ADay: Integer                     - 用来容纳计算结果的年、月、日

   返回值：Boolean                                        - 返回是否计算成功
}

function Compare2Day(Year1, Month1, Day1, Year2, Month2, Day2: Integer): Integer;
{* 比较两个公历日期，前者大于、等于、小于后者时分别返回 1、0、-1，大于指更靠近未来。

   参数：
     Year1, Month1, Day1: Integer         - 待判断的第一个公历年、月、日
     Year2, Month2, Day2: Integer         - 待判断的第二个公历年、月、日

   返回值：Integer                        - 返回比较结果
}

function Compare2LunarDay(Year1, Month1, Day1: Integer; IsLeap1: Boolean;
  Year2, Month2, Day2: Integer; IsLeap2: Boolean): Integer;
{* 比较两个农历日期（包括闰月信息），前者大于、等于、小于后者时分别返回 1、0、-1，大于指更靠近未来。

   参数：
     Year1, Month1, Day1: Integer         - 待判断的第一个农历年、月、日
     IsLeap1: Boolean                     - 第一个农历是否闰月
     Year2, Month2, Day2: Integer         - 待判断的第二个农历年、月、日
     IsLeap2: Boolean                     - 第二个农历是否闰月

   返回值：Integer                        - 返回比较结果
}

function GetYearSeperatedByLiChun(AYear, AMonth, ADay: Integer): Integer;
{* 根据公历年月日，返回该日所属的以立春分割的年份，也就是说立春日后是今年，否则为去年。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回所属公历年
}

function GetEquStandardDays(AYear, AMonth, ADay: Integer): Integer;
{* 获得某公历日的等效标准日数，似乎是离格利高里历往前推到儒略历公元元年元月 0 日的日数。
   注意由于内部计算特殊场合的要求，该函数的年参数必须连续且出现 0，也即公元前 1 年为 0。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Integer                        - 返回等效标准日数
}

function GetDayFromEquStandardDays(EquDays: Integer;
  out AYear, AMonth, ADay: Integer): Boolean;
{* 获得等效标准日数对应的某公历日，返回是否获取成功。
   目前不支持公元元年元月 2 日之前的日期也就是不支持负的等效标准日。
   注意由于内部计算特殊场合的要求，该函数返回的年数连续且出现 0，也即公元前 1 年为 0。

   参数：
     EquDays: Integer                     - 待计算的等效标准日数
     out AYear, AMonth, ADay: Integer     - 用来容纳计算结果的年、月、日

   返回值：Boolean                        - 返回是否计算成功
}

function GetJulianDate(AYear, AMonth, ADay: Integer): Extended; overload;
{* 获得某公历日中午 12 点的儒略日数，也即以儒略历的公元前 4713 年 1 月 1 日
   中午 12 点为起点的日数（该年是儒略历闰年），一般是个整数。注意无公元 0 年。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Extended                       - 返回儒略日数
}

function GetJulianDate(AYear, AMonth, ADay: Integer;
  AHour, AMinute, ASecond: Integer): Extended; overload;
{* 获得某公历日期时刻的儒略日数。注意无公元 0 年。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日
     AHour, AMinute, ASecond: Integer     - 待计算的时、分、秒

   返回值：Extended                       - 返回儒略日数
}

function GetModifiedJulianDate(AYear, AMonth, ADay: Integer): Extended; overload;
{* 获得某公历日中午 12 点的约化儒略日数，也即以格里高利历的公元 1858 年 11 月 17 日
   0 点为起点的日数，小数部分一般是 0.5。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日

   返回值：Extended                       - 返回约化儒略日数
}

function GetModifiedJulianDate(AYear, AMonth, ADay: Integer;
  AHour, AMinute, ASecond: Integer): Extended; overload;
{* 获得某公历日期时刻的约化儒略日数。

   参数：
     AYear, AMonth, ADay: Integer         - 待计算的公历年、月、日
     AHour, AMinute, ASecond: Integer     - 待计算的时、分、秒

   返回值：Extended                       - 返回约化儒略日数
}

function GetDayFromJulianDate(JD: Extended; out AYear, AMonth, ADay: Integer): Boolean;
{* 获得某儒略日数对应哪一天的公历年月日，整数对应当日正午。

   参数：
     JD: Extended                         - 待计算的儒略日数，应当尽量传整数值
     out AYear, AMonth, ADay: Integer     - 返回的公历年、月、日

   返回值：Boolean                        - 返回是否计算成功
}

function GetDayFromModifiedJulianDate(MJD: Extended; out AYear, AMonth, ADay: Integer): Boolean;
{* 获得某约化儒略日数对应的公历年月日，小数部分应当是 0.5，对应当日正午。

   参数：
     MJD: Extended                        - 待计算的约化儒略日数，小数部分应当是 0.5
     out AYear, AMonth, ADay: Integer     - 返回的公历年、月、日

   返回值：Boolean                        - 返回是否计算成功
}

implementation

resourcestring
  SCnErrorDateIsInvalid = 'Date is Invalid: %d-%d-%d.';
  SCnErrorLunarDateIsInvalid = 'Lunar Date is Invalid: %d-%d-%d, MonthLeap %d.';
  SCnErrorConvertLunarDate = 'Date is Invalid for Lunar Conversion: %d-%d-%d.';
  SCnErrorTimeIsInvalid = 'Time is Invalid: %d:%d:%d.';
  SCnErrorYearIsInvalid = 'Year is Invalid: 0';
  SCnErrorJieQiIndexIsInvalid = 'JieQi Index is Invalid %d';

const
  RADS = 0.0174532925;

  SCnTaiXuanPeiShuArray: array[0..5] of Integer =
    (9, 8, 7, 6, 5, 4);
  {* 干支的太玄配数数组，供内部计算干支的太玄配数从而计算纳音五行用}

  SCnLunar28XiuNumber: array[1..12] of array[1..30] of Byte = (
  {* 农历月、日对应的二十八宿数组，供查表用。注意里头没有 8 牛}
    (12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14),
    (14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16),
    (16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18),
    (18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
    (20, 21, 22, 23, 24, 25, 26, 27, 0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22),
    (22, 23, 24, 25, 26, 27, 0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24),
    (25, 26, 27, 0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27),
    (0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 0, 1, 2),
    (2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 0, 1, 2, 3, 4),
    (4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 0, 1, 2, 3, 4, 5, 6),
    (7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 0, 1, 2, 3, 4, 5, 6, 7, 9, 10),
    (10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12)
  );

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
    751, 751, 751, 752, 752, 752,
    {此处 1199 到 1200 出现 2 跳变，属原始数据错误，开始都减一}
    753, 753, 754, 754, 754, 755, 755, 755,
    756, 756, 756, 757, 757, 758, 758, 758, 759, 759, 759, 760, 760, 761,
    761, 761, 762, 762, 762, 763, 763, 763, 764, 764, 765, 765, 765, 766,
    766, 766, 767, 767, 768, 768, 768, 769, 769, 769, 770, 770, 770, 771,
    771, 772, 772, 772, 773, 773, 773, 774, 774, 775, 775, 775, 776, 776,
    776, 777, 777, 777, 778, 778, 779, 779, 779, 780, 780, 780, 781, 781,
    782, 782, 782, 783, 783, 783, 784, 784, 784, 785, 785, 786, 786, 786,
    787, 787, 787, 788, 788, 789, 789, 789, 790, 790, 790, 791, 791, 791,
    792, 792, 793, 793, 793, 794, 794, 794, 795, 795, 795, 796, 796, 797,
    797, 797, 798, 798, 798, 799, 799, 800, 800, 800, 801, 801, 801, 802,
    802, 803, 803, 803, 804, 804, 804, 805, 805, 805, 806, 806, 807, 807,
    807, 808, 808, 808, 809, 809, 809, 810, 810, 811, 811, 811, 812, 812,
    812, 813, 813, 814, 814, 814, 815, 815, 815, 816, 816, 817, 817, 817,
    818, 818, 818, 819, 819, 819, 820, 820, 821, 821, 821, 822, 822, 822,
    823, 823, 824, 824, 824, 825, 825, 825, 826, 826, 826, 827, 827, 828,
    828, 828, 829, 829, 829, 830, 830, 831, 831, 831, 832, 832, 832, 833,
    833, 833, 834, 834, 835, 835, 835, 836, 836, 836, 837, 837, 838, 838,
    838, 839, 839, 839, 840, 840, 840, 841, 841, 842, 842, 842, 843, 843,
    843, 844, 844, 844, 845, 845, 846, 846, 846, 847, 847, 847, 848, 848,
    849, 849, 849, 850, 850, 850, 851, 851, 851, 852, 852, 853, 853, 853,
    854, 854, 854, 855, 855, 856, 856, 856, 857, 857, 857, 858, 858, 858,
    859, 859, 860, 860, 860, 861, 861, 861, 862, 862, 863, 863,
    {此处 1499 到 1500 出现往回跳变，属原始数据错误，结束减一} 863, 864,
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
    0~3648 共 3649 项，包括公元前 850 年到公元前一年的 850 项及公元元年到公元 2799 年的 2799 项，
    无不存在的公元 0 年}

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
    'a0070030b0' + '0' + '80050010a008005001090050020a0060030c0080040010a0060030c0090' +
    // 这里 70030b008005 中，b008 的第二个 0 是公元 0 年的非法数据，搁这占位用
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
    '02090060030b0080040020a0060040b0080040010b0060030b0070050030a006004002' +
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
    共 3650 项，竟然比上面的多一项，原因是为了方便直接按公元年份 +849 做下标访问，
    内部多塞了个公元 0 年的 0 值，费解但目测不影响}

  {* 二十四节气常量值供内部使用（按公历年出现顺序排列，小寒为第 1 个节气）}
  CN_JIEQI_XIAOHAN     = 1;    // 小寒
  CN_JIEQI_DAHAN       = 2;    // 大寒
  CN_JIEQI_LICHUN      = 3;    // 立春
  CN_JIEQI_YUSHUI      = 4;    // 雨水
  CN_JIEQI_JINGZHE     = 5;    // 惊蛰
  CN_JIEQI_CHUNFEN     = 6;    // 春分
  CN_JIEQI_QINGMING    = 7;    // 清明
  CN_JIEQI_GUYU        = 8;    // 谷雨
  CN_JIEQI_LIXIA       = 9;    // 立夏
  CN_JIEQI_XIAOMAN     = 10;   // 小满
  CN_JIEQI_MANGZHONG   = 11;   // 芒种
  CN_JIEQI_XIAZHI      = 12;   // 夏至
  CN_JIEQI_XIAOSHU     = 13;   // 小暑
  CN_JIEQI_DASHU       = 14;   // 大暑
  CN_JIEQI_LIQIU       = 15;   // 立秋
  CN_JIEQI_CHUSHU      = 16;   // 处暑
  CN_JIEQI_BAILU       = 17;   // 白露
  CN_JIEQI_QIUFEN      = 18;   // 秋分
  CN_JIEQI_HANLU       = 19;   // 寒露
  CN_JIEQI_SHUANGJIANG = 20;   // 霜降
  CN_JIEQI_LIDONG      = 21;   // 立冬
  CN_JIEQI_XIAOXUE     = 22;   // 小雪
  CN_JIEQI_DAXUE       = 23;   // 大雪
  CN_JIEQI_DONGZHI     = 24;   // 冬至

  CN_JIEQI_TOTAL_COUNT = 24;   // 一共 24 个节气

type
  TCnLunarDateSingleMonthFix = packed record
  {* 因古代观测精度限制，针对单个农历月的公农历转换修正一天的数据，暂时没处理跨年的情况}
    Year: Integer;         // 偏差所在的年份
    Month: Integer;        // 偏差开始的月份，结束是下一个月
    StartDay: Integer;     // 偏差开始的日期
    EndDay: Integer;       // 下个月偏差结束的日期
    IncOne: Boolean;       // True 表示加一天，False 表示减一天
  end;

const
  CN_LUNAR_SINGLE_MONTH_FIX: array[0..8] of TCnLunarDateSingleMonthFix = (
  {* 历史上因观测偏差导致的单个农历月首的单日偏差修正}
    (Year:  244; Month:  4; StartDay: 24; EndDay: 23; IncOne: False),
    (Year:  245; Month:  1; StartDay: 15; EndDay: 13; IncOne: True),
    (Year:  245; Month:  5; StartDay: 13; EndDay: 11; IncOne: False),
    (Year:  245; Month:  7; StartDay: 11; EndDay:  9; IncOne: False),
    (Year:  246; Month:  2; StartDay:  3; EndDay:  4; IncOne: True),
    (Year: 1199; Month:  3; StartDay: 28; EndDay: 26; IncOne: False),
    (Year: 1914; Month: 11; StartDay: 17; EndDay: 16; IncOne: True),
    (Year: 1924; Month:  3; StartDay:  5; EndDay:  3; IncOne: True),
    (Year: 2018; Month: 11; StartDay:  7; EndDay:  6; IncOne: False)
  );

// 无公元元年的公历年份，转换为内部连续的包含 0 的年份，负值加一
procedure NonZeroYearToZeroYear(var AYear: Integer);
begin
  if AYear = 0 then
    raise ECnDateTimeException.Create(SCnErrorYearIsInvalid);

  if AYear < 0 then
    Inc(AYear);
end;

// 内部连续的包含 0 的年份，转换为无公元元年的公历年份，非正值减一
procedure ZeroYearToNonZeroYear(var AYear: Integer);
begin
  if AYear <= 0 then
    Dec(AYear);
end;

//==============================================================================
// 以下是日出日落计算的内容
//==============================================================================

function HoursMin(Hours: Extended): TDateTime;
begin
  Result := Hours / 24;
end;

function IntPart(X: Extended): Extended;
begin
  if X > 0 then
    Result := Floor(X)
  else
    Result := Ceil(X);
end;

function Range360(X: Extended): Extended;
var
  A: Extended;
begin
  A := X / 360;
  Result := 360 * (A - IntPart(A));
  if Result < 0 then
    Result := Result + 360;
end;

// 日出日落专用的计算约化儒略日
function Mjd(Year, Month, Day, Hour: Integer): Extended;
var
  A, B: Extended;
begin
  if Month <= 2 then
  begin
    Month := Month + 12;
    Year := Year - 1;
  end;

  A := 10000.0 * Year + 100.0 * Month + Day;
  if A <= 15821004.1 then
  begin
    B := -2 * Floor((Year + 4716) / 4) - 1179;
  end
  else
  begin
    B := Floor(Year / 400) - Floor(Year / 100) + Floor(Year / 4);
  end;

  A := 365.0 * Year - 679004.0;
  Result := A + B + Floor(30.6001 * (Month + 1)) + Day + Hour / 24.0;
end;

procedure Quad(ym, yz, yp: Extended; var nz, z1, z2, xe, ye: Extended);
var
  A, B, c, dis, dx: Extended;
begin
  nz := 0;
  A := 0.5 * (ym + yp) - yz;
  B := 0.5 * (yp - ym);
  c := yz;
  xe := -B / (2 * A);
  ye := (A * xe + B) * xe + c;
  dis := B * B - 4.0 * A * c;
  if dis > 0 then
  begin
    dx := 0.5 * Sqrt(dis) / Abs(A);
    z1 := xe - dx;
    z2 := xe + dx;
    if Abs(z1) <= 1.0 then
      nz := nz + 1;
    if Abs(z2) <= 1.0 then
      nz := nz + 1;
    if z1 < -1.0 then
      z1 := z2;
  end;
end;

function Lmst(AMjd, Glong: Extended): Extended;
var
  Lst, T, D: Extended;
begin
  D := AMjd - 51544.5;
  T := D / 36525.0;
  Lst := Range360(280.46061837 + 360.98564736629 * D + 0.000387933 * T * T -
    T * T * T / 38710000);
  Result := Lst / 15.0 + Glong / 15;
end;

procedure MiniSun(T: Extended; var ADec, Ra: Extended);
const
  P2 = 6.283185307;
  CosEps = 0.91748;
  SinEps = 0.39778;
var
  L, M, DL, SL, X, Y, Z, RHO: Extended;
begin
  M := P2 * Frac(0.993133 + 99.997361 * T);
  DL := 6893.0 * Sin(M) + 72.0 * Sin(2 * M);
  L := P2 * Frac(0.7859453 + M / P2 + (6191.2 * T + DL) / 1296000);
  SL := Sin(L);
  X := Cos(L);
  Y := CosEps * SL;
  Z := SinEps * SL;
  RHO := Sqrt(1 - Z * Z);
  ADec := (360.0 / P2) * ArcTan2(Z, RHO);
  Ra := (48.0 / P2) * ArcTan2(Y, (X + RHO));
  if Ra < 0 then
    Ra := Ra + 24;
end;

function SinAlt(Mjd0, hour, Glong, Cglat, Sglat: Extended): Extended;
var
  AMjd, T, Ra, ADec, Tau, Salt: Extended;
begin
  AMjd := Mjd0 + hour / 24.0;
  T := (AMjd - 51544.5) / 36525.0;
  MiniSun(T, ADec, Ra);
  Tau := 15.0 * (Lmst(AMjd, Glong) - Ra);
  Salt := Sglat * Sin(RADS * ADec) + Cglat * Cos(RADS * ADec) * Cos(RADS * Tau);
  Result := Salt;
end;

function GetZTTime(Mjd, Tz, Glong: Extended): Extended;
var
  sinho, date, ym, yz, utrise, utset: Extended;
  yp, ye, nz, hour, z1, z2, xe: Extended;
begin
  sinho := Sin(RADS * -0.833);
  date := Mjd - Tz / 24;
  hour := 1.0;
  ym := SinAlt(date, hour - 1.0, Glong, 1, 0) - sinho;

  utrise := 0;
  utset := 0;
  while hour < 25 do
  begin
    yz := SinAlt(date, hour, Glong, 1, 0) - sinho;
    yp := SinAlt(date, hour + 1.0, Glong, 1, 0) - sinho;
    Quad(ym, yz, yp, nz, z1, z2, xe, ye);

    if nz = 1 then
    begin
      if ym < 0.0 then
        utrise := hour + z1
      else
        utset := hour + z1;
    end;

    if nz = 2 then
    begin
      if ye < 0.0 then
      begin
        utrise := hour + z2;
        utset := hour + z1;
      end
      else
      begin
        utrise := hour + z1;
        utset := hour + z2;
      end;
    end;
    ym := yp;
    hour := hour + 2.0;
  end;

  Result := (utrise + utset) / 2;
  if Result < utrise then
    Result := Result + 12;
  if Result > 24 then
    Result := Result - 24;
end;

function DoSunCalc(AMjd: Extended; Glong, Glat: Extended;
  Tz: Integer; out RiseTime, TransitTime, SetTime: TDateTime):
  TCnSunRiseSetType;
var
  sinho, sglat, cglat: Extended;
  yz, yp, ym, nz, z1, z2, xe, ye: Extended;
  utrise, utset, zt: Extended;
  date, hour: Extended;
  rise, sett, above: Boolean;
begin
  sinho := Sin(RADS * -0.833);
  sglat := Sin(RADS * Glat);
  cglat := Cos(RADS * Glat);
  Date := AMjd - Tz / 24;

  rise := False;
  sett := False;
  above := False;
  hour := 1.0;
  utrise := 0;
  utset := 0;
  ym := SinAlt(date, hour - 1.0, Glong, cglat, sglat) - sinho;
  if ym > 0.0 then
    above := True;

  while (hour < 25) and (not sett or not rise) do
  begin
    yz := SinAlt(date, hour, Glong, cglat, sglat) - sinho;
    yp := SinAlt(date, hour + 1.0, Glong, cglat, sglat) - sinho;
    Quad(ym, yz, yp, nz, z1, z2, xe, ye);
    if nz = 1 then
    begin
      if ym < 0.0 then
      begin
        utrise := hour + z1;
        rise := True;
      end
      else
      begin
        utset := hour + z1;
        sett := True;
      end;
    end;

    if nz = 2 then
    begin
      if ye < 0.0 then
      begin
        utrise := hour + z2;
        utset := hour + z1;
      end
      else
      begin
        utrise := hour + z1;
        utset := hour + z2;
      end;
    end;

    ym := yp;
    hour := hour + 2.0;
  end;

  RiseTime := -1;
  TransitTime := -1;
  SetTime := -1;
  if rise or sett then
  begin
    if rise then
      RiseTime := HoursMin(utrise);

    zt := GetZTTime(AMjd, Tz, Glong);
    TransitTime := HoursMin(zt);

    if sett then
      SetTime := HoursMin(utset);

    Result := stNormal;
  end
  else if above then
  begin
    zt := GetZTTime(AMjd, Tz, Glong);
    TransitTime := HoursMin(zt);

    Result := stAllwaysUp;
  end
  else
  begin
    Result := stAllwaysDown;
  end;
end;

// 计算日出日落时间
function GetSunRiseSetTime(ADate: TDateTime; Longitude, Latitude: Extended;
  ZoneTime: Integer; out RiseTime, TransitTime, SetTime: TDateTime):
  TCnSunRiseSetType;
var
  Year, Month, Day: Word;
  Mg: Extended;
begin
  try
    DecodeDate(ADate, Year, Month, Day);
    Mg := Mjd(Year, Month, Day, 0);
    Result := DoSunCalc(Mg, Longitude, Latitude, ZoneTime, RiseTime, TransitTime, SetTime);
  except
    Result := stError;
  end;
end;

// 从数字获得五行名, 0-4 对应 金木水火土
function Get5XingFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if AValue in [0..4] then
    Result := SCn5XingArray[AValue];
end;

// 从数字获得十二建名, 0-11
function Get12JianFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if AValue in [0..11] then
    Result := SCn12JianArray[AValue];
end;

// 从数字获得三伏名, 0-2
function Get3FuFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if AValue in [0..2] then
    Result := SCn3FuArray[AValue];
end;

// 从数字获得阴阳名, 0-1
function GetYinYangFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if AValue in [0..1] then
    Result := SCnYinYangArray[AValue];
end;

// 从数字获得天干名,0-9
function GetTianGanFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if (AValue >= 0)  and (AValue < 10) then
    Result := SCnTianGanArray[AValue];
end;

// 从数字获得地支名, 0-11
function GetDiZhiFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if (AValue >= 0)  and (AValue < 12) then
    Result := SCnDiZhiArray[AValue];
end;

// 从数字获得天干地支名, 0-59
function GetGanZhiFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if (AValue >= 0)  and (AValue < 60) then
    Result := SCnGanZhiArray[AValue];
end;

// 从地支数字获得十二太岁名, 0-11
function Get12TaiSuiFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if (AValue >= 0)  and (AValue < 12) then
    Result := SCn12TaiSuiArray[AValue];
end;

// 从数字获得六十太岁名, 0-59
function Get60TaiSuiFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if (AValue >= 0)  and (AValue < 60) then
    Result := SCn60TaiSuiArray[AValue];
end;

// 从数字获得生肖名, 0-11
function GetShengXiaoFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if (AValue >= 0)  and (AValue < 12) then
    Result := SCnShengXiaoArray[AValue];
end;

// 从数字获得节气名, 0-23
function GetJieQiFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if (AValue >= 0)  and (AValue < 24) then
    Result := SCnJieQiArray[AValue];
end;

// 从数字获得星座名, 0-11
function GetXingZuoFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if (AValue >= 0)  and (AValue < 12) then
    Result := SCnXingZuoArray[AValue];
end;

// 从数字获得二十八宿名, 0-27
function Get28XiuFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if (AValue >= 0)  and (AValue < 28) then
    Result := SCn28XiuArray[AValue];
end;

// 从数字获得二十八宿完整名, 0-27
function Get28XiuLongFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if (AValue >= 0)  and (AValue < 28) then
    Result := SCn28XiuLongArray[AValue];
end;

// 从数字获得农历月名称, 1-12
function GetLunarMonthFromNumber(const AMonth: Integer; IsLeap: Boolean): string;
begin
  Result := '';
  if (AMonth >= 1) and (AMonth <= 12) then
  begin
    Result := SCnLunarMonthNameArray[AMonth - 1] + SCnLunarMonthName;
    if IsLeap then
      Result := SCnLunarMonthLeapName + Result;
  end;
end;

// 从数字获得农历日名称, 1-30
function GetLunarDayFromNumber(const ADay: Integer): string;
var
  D1, D2: Integer;
begin
  Result := '';
  if ADay in [1..30] then
  begin
    D2 := ADay div 10; // 十位
    D1 := ADay mod 10; // 个位
    // 部分修正
    if D1 = 0 then
    begin
      case D2 of
      1:
        begin
          D2 := 0;
          D1 := 10; // 初十
        end;
      2, 3:
        begin
          Inc(D2, 2);
          D1 := 10; // 一般不单独用廿、卅，而是二十三十。
        end;
      end;
    end;
    Result := SCnLunarNumber2Array[D2] + SCnLunarNumber1Array[D1 - 1];
  end;
end;

// 从天干获得其阴阳, 0-9 转换成 0-1
function GetYinYangFromGan(const Gan: Integer): Integer;
begin
  Result := -1;
  if Gan in [0..9] then // 甲阳乙阴丙阳丁阴，以此类推
    Result := 1 - (Gan mod 2);
end;

// 从地支获得其阴阳, 0-11 转换成 0-1
function GetYinYangFromZhi(const Zhi: Integer): Integer;
begin
  Result := -1;
  if Zhi in [0..11] then // 子阴丑阳寅阴卯阳，以此类推
    Result := 1 - (Zhi mod 2);
end;

// 将天干地支组合成干支，0-9 0-11 转换成 0-59。注意是六十轮排，不是任意两个干支都能组合
function CombineGanZhi(Gan, Zhi: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  if (Gan in [0..9]) and (Zhi in [0..11]) then
  begin
    for I := 0 to 6 do
    begin
      if (I * 10 + Gan) mod 12 = Zhi then
      begin
        Result := I * 10 + Gan;
        Exit;
      end;
    end;
  end;
end;

// 将干支拆分成天干地支，0-59 转换成 0-9 0-11
function ExtractGanZhi(GanZhi: Integer; out Gan: Integer; out Zhi: Integer): Boolean;
begin
  if GanZhi in [0..59] then
  begin
    Result := True;
    Gan := GanZhi mod 10;
    Zhi := GanZhi mod 12;
  end
  else
  begin
    Result := False;
    Gan := -1;
    Zhi := -1;
  end;
end;

// 获得某干的五行，0-4 对应 金木水火土
function Get5XingFromGan(const Gan: Integer): Integer;
begin
  case Gan div 2 of
    0: Result := 1; // 甲乙木
    1: Result := 3; // 丙丁火
    2: Result := 4; // 戊己土
    3: Result := 0; // 庚辛金
    4: Result := 2; // 壬癸水
  else
    Result := -1;
  end;
end;

// 获得某支的五行，0-4 对应 金木水火土
function Get5XingFromZhi(const Zhi: Integer): Integer;
begin
  case Zhi of
    8, 9: Result := 0;
    2, 3: Result := 1;
    0,11: Result := 2;
    5, 6: Result := 3;
    1, 4, 7, 10: Result := 4;
  else
    Result := -1;
  end;
end;

// 从天干获得太玄配数值，内部使用
function GetTaiXuanPeiShuFromGan(const Gan: Integer): Integer;
begin
  Result := -1;
  if Gan in [0..9] then
    Result := SCnTaiXuanPeiShuArray[Gan mod 5];
end;

// 从地支获得太玄配数值，内部使用
function GetTaiXuanPeiShuFromZhi(const Zhi: Integer): Integer;
begin
  Result := -1;
  if Zhi in [0..11] then
    Result := SCnTaiXuanPeiShuArray[Zhi mod 6];
end;

// 获得某干支的纳音五行（短），0-4 对应 金木水火土
function Get5XingFromGanZhi(const GanZhi: Integer): Integer; overload;
var
  Gan, Zhi: Integer;
begin
  ExtractGanZhi(GanZhi, Gan, Zhi);
  Result := Get5XingFromGanZhi(Gan, Zhi);
end;

// 获得某干支的纳音五行（短），0-4 对应 金木水火土
function Get5XingFromGanZhi(Gan, Zhi: Integer): Integer; overload;
var
  TaiXuan1, TaiXuan2, TaiXuan3, TaiXuan4: Integer; // 四个太玄配数
begin
  // 此处干支必须同为奇数或同为偶，其余配对非法
  Result := -1;
  if (Gan + Zhi) mod 2 = 0 then
  begin
    TaiXuan1 := GetTaiXuanPeiShuFromGan(Gan);
    TaiXuan2 := GetTaiXuanPeiShuFromZhi(Zhi);
    if Gan mod 2 = 0 then
    begin
      // 偶，为阳对，取阴对
      Inc(Gan);
      Inc(Zhi);
    end
    else // 奇，为阴对，取阳对
    begin
      Dec(Gan);
      Dec(Zhi);
    end;
    TaiXuan3 := GetTaiXuanPeiShuFromGan(Gan);
    TaiXuan4 := GetTaiXuanPeiShuFromZhi(Zhi);

    Result := (TaiXuan1 + TaiXuan2 + TaiXuan3 + TaiXuan4) mod 5;
    case Result of // 重新映射，一为火，二为土，三为木，四为金，五（0）为水。
      0: Result := 2;
      1: Result := 3;
      2: Result := 4;
      3: Result := 1;
      4: Result := 0;
    end;
  end;
end;

// 获得某公历日的纳音五行（短），0-4 对应 金木水火土
function Get5XingFromDay(AYear, AMonth, ADay: Integer): Integer;
var
  Gan, Zhi: Integer;
begin
  ExtractGanZhi(GetGanZhiFromDay(AYear, AMonth, ADay), Gan, Zhi);
  Result := Get5XingFromGanZhi(Gan, Zhi);
end;

// 获得某干支的纳音五行（长），返回字符串
function Get5XingLongFromGanZhi(const GanZhi: Integer): string; overload;
var
  I: Integer;
begin
  I := GanZhi div 2;
  if I in [0..29] then
    Result := SCnNaYinWuXingArray[I]
  else
    Result := '';
end;

// 获得某干支的纳音五行（长），返回字符串
function Get5XingLongFromGanZhi(Gan, Zhi: Integer): string; overload;
var
  GanZhi: Integer;
begin
  GanZhi := CombineGanZhi(Gan, Zhi);
  Result := Get5XingLongFromGanZhi(GanZhi);
end;
// 获得某公历日的纳音五行（长），返回字符串
function Get5XingLongFromDay(AYear, AMonth, ADay: Integer): string;
var
  Gan, Zhi: Integer;
begin
  ExtractGanZhi(GetGanZhiFromDay(AYear, AMonth, ADay), Gan, Zhi);
  Result := Get5XingLongFromGanZhi(Gan, Zhi);
end;

// 获得某地支的另两个三合地支
function Get3HeFromZhi(const Zhi: Integer; out He1: Integer;
  out He2: Integer): Boolean;
begin
  // 三合是互相差 4 的三个地支，也就是申子辰、寅午戌、巳酉丑、卯亥未，
  // 或者说猴鼠龙、虎马狗、蛇鸡牛、兔猪羊
  Result := False;
  if Zhi in [0..11] then
  begin
    case Zhi of
      0:  begin He1 := 8;  He2 := 4;  end;
      1:  begin He1 := 5;  He2 := 9;  end;
      2:  begin He1 := 6;  He2 := 10; end;
      3:  begin He1 := 11; He2 := 7;  end;
      4:  begin He1 := 8;  He2 := 0;  end;
      5:  begin He1 := 9;  He2 := 1;  end;
      6:  begin He1 := 2;  He2 := 10; end;
      7:  begin He1 := 3;  He2 := 11; end;
      8:  begin He1 := 0;  He2 := 4;  end;
      9:  begin He1 := 5;  He2 := 1;  end;
      10: begin He1 := 2;  He2 := 6;  end;
      11: begin He1 := 3;  He2 := 7;  end;
    else
      He1 := -1;
      He2 := -1;
    end;
    Result := True;
  end;
end;

// 根据公历日期判断当时历法
function GetCalendarType(AYear, AMonth, ADay: Integer): TCnCalendarType;
begin
  if AYear > 1582 then
    Result := ctGregorian
  else if AYear < 1582 then
    Result := ctJulian
  else if AMonth < 10 then
    Result := ctJulian
  else if (AMonth = 10) and (ADay <= 4) then
    Result := ctJulian
  else if (AMonth = 10) and (ADay in [5..14]) then
    Result := ctInvalid
  else
    Result := ctGregorian;
end;

// 返回某公历是否闰年
function GetIsLeapYear(AYear: Integer): Boolean;
begin
  if GetCalendarType(AYear, 1, 1) = ctGregorian then
    Result := (AYear mod 4 = 0) and ((AYear mod 100 <> 0) or (AYear mod 400 = 0))
  else if AYear > 0 then
    Result := (AYear mod 4 = 0)
  else if AYear < 0 then // 需要独立判断公元前的原因是没有公元 0 年
    Result := (AYear - 3) mod 4 = 0
  else
    raise ECnDateTimeException.Create(SCnErrorYearIsInvalid);
end;

// 取本月天数，不考虑 1582 年 10 月的特殊情况
function GetMonthDays(AYear, AMonth: Integer): Integer;
begin
  case AMonth of
    1,3,5,7,8,10,12:
      Result := 31;
    4,6,9,11:
      Result:= 30;
    2:// 闰年
      if (AYear <> 0) and GetIsLeapYear(AYear) then
        Result := 29
      else
        Result := 28 // 没有公元 0 年，当成平年处理
  else
    Result := 0;
  end;
end;

// 获得某农历年的历史上增加的闰月，返回 1~12 对应一月到十二月，返回 0 表示无额外闰月
function GetLunarAdditionalLeapMonth(AYear: Integer): Integer;
begin
  if (AYear in [23, 239]) then // 这俩农历年加了两个十二月
    Result := 12
  else
    Result := 0;
end;

// 取农历年的某月天数
function GetLunarMonthDays(ALunarYear, ALunarMonth: Integer;
  IsLeapMonth: Boolean = False): Integer;
var
  EquDay1, EquDay2: Integer;
  AYear, AMonth, ADay: Integer;
  ALeap: Boolean;
begin
  Result := -1;
  if IsLeapMonth and (GetLunarLeapMonth(ALunarYear) <> ALunarMonth)
    and (GetLunarAdditionalLeapMonth(ALunarYear) <> ALunarMonth) then
    Exit; // 该年无此闰月或额外闰月则退出

  if not GetDayFromLunar(ALunarYear, ALunarMonth, 1, IsLeapMonth, AYear, AMonth, ADay) then
    Exit;

  EquDay1 := GetEquStandardDays(AYear, AMonth, ADay);

  ALeap := False;
  if (GetLunarLeapMonth(ALunarYear) = ALunarMonth) or
    (GetLunarAdditionalLeapMonth(ALunarYear) = ALunarMonth) then // 这个月在本年内是有个闰月的
  begin
    if IsLeapMonth then // 如果输入就是闰月，则后推一个月
    begin
      Inc(ALunarMonth);
      if ALunarMonth > 12 then
      begin
        Dec(ALunarMonth, 12);
        Inc(ALunarYear);
      end;
    end
    else
      ALeap := True; // 后一个月是闰月
  end
  else
  begin
    Inc(ALunarMonth);
    if ALunarMonth > 12 then
    begin
      Dec(ALunarMonth, 12);
      Inc(ALunarYear);
    end;
  end;

  if not GetDayFromLunar(ALunarYear, ALunarMonth, 1, ALeap, AYear, AMonth, ADay) then
    Exit;

  EquDay2 := GetEquStandardDays(AYear, AMonth, ADay);
  Result := EquDay2 - EquDay1;
end;

// 返回公历日期是否合法
function GetDateIsValid(AYear, AMonth, ADay: Integer): Boolean;
begin
  Result := (AYear <> 0) and (AMonth in [1..12]) and (ADay > 0)
    and (ADay <= GetMonthDays(AYear, AMonth));
  if Result and (AYear = 1582) and (AMonth = 10) then
    Result := not (ADay in [5..14]);
end;

// 判断公历日期是否合法，不合法则抛出异常
procedure ValidDate(AYear, AMonth, ADay: Integer);
begin
  if not GetDateIsValid(AYear, AMonth, ADay) then
    raise ECnDateTimeException.CreateFmt(SCnErrorDateIsInvalid, [AYear, AMonth, ADay]);
end;

// 返回农历日期是否合法
function GetLunarDateIsValid(ALunarYear, ALunarMonth, ALunarDay: Integer;
  IsLeapMonth: Boolean): Boolean;
begin
  Result := False;
  if (ALunarYear = 0) or not (ALunarMonth in [1..12]) then
    Exit;

  if ALunarDay > 30 then
    Exit;

  if IsLeapMonth and (GetLunarLeapMonth(ALunarYear) <> ALunarMonth)
    and (GetLunarAdditionalLeapMonth(ALunarYear) <> ALunarMonth) then
    Exit; // 该年无此闰月或额外闰月则退出

  // 判断大小月数是否超界
  if ALunarDay = 30 then
  begin
    if ALunarDay > GetLunarMonthDays(ALunarYear, ALunarMonth, IsLeapMonth) then
      Exit;
  end;

  Result := True;
end;

// 判断农历日期是否合法，不合法则抛出异常
procedure ValidLunarDate(ALunarYear, ALunarMonth, ALunarDay: Integer; IsLeapMonth: Boolean);
begin
  if not GetLunarDateIsValid(ALunarYear, ALunarMonth, ALunarDay, IsLeapMonth) then
    raise ECnDateTimeException.CreateFmt(SCnErrorLunarDateIsInvalid,
      [ALunarYear, ALunarMonth, ALunarDay, Integer(IsLeapMonth)]);
end;

// 返回时间是否合法
function GetTimeIsValid(AHour, AMinitue, ASecond: Integer): Boolean;
begin
  Result := (AHour in [0..23]) and (AMinitue in [0..59]) and (ASecond in [0..59]);
end;

// 判断时间是否合法，不合法则抛出异常
procedure ValidTime(AHour, AMinitue, ASecond: Integer);
begin
  if not GetTimeIsValid(AHour, AMinitue, ASecond) then
    raise ECnDateTimeException.CreateFmt(SCnErrorTimeIsInvalid, [AHour, AMinitue, ASecond]);
end;

// 公历年月日往后步进一天，考虑各种闰年、格里高利历删 10 天等因素
procedure StepToNextDay(var AYear, AMonth, ADay: Integer; ZeroYear: Boolean);
var
  LY: Integer;
begin
  if not ZeroYear then
    ValidDate(AYear, AMonth, ADay);

  if (AYear = 1582) and (AMonth = 10) and (ADay = 4) then
  begin
    ADay := 15;
    Exit;
  end
  else
  begin
    if ZeroYear and (AYear <= 0) then   // GetIsLeapYear 和 GetMonthDays 只接受非 0 年数
      LY := AYear - 1
    else
      LY := AYear;

    if (AMonth = 2) and (ADay = 28) then // 处理闰年的 2 月 29 日
    begin
      if GetIsLeapYear(LY) then
        ADay := 29
      else
      begin
        ADay := 1;
        Inc(AMonth);
      end;
      Exit;
    end
    else
    begin
      if ADay >= GetMonthDays(LY, AMonth) then // 月底，进月
      begin
        ADay := 1;
        Inc(AMonth);

        if AMonth = 13 then  // 超月，进年
        begin
          AMonth := 1;

          if not ZeroYear and (AYear = -1) then // 公元前一年到公元元年
            AYear := 1
          else
            Inc(AYear);
        end;
      end
      else
      begin
        Inc(ADay); // 非月底，加一天就行
      end;
    end;
  end;
end;

// 比较两个公历日期，1 >=< 2 分别返回 1、0、-1
function Compare2Day(Year1, Month1, Day1, Year2, Month2, Day2: Integer): Integer;
begin
  ValidDate(Year1, Month1, Day1);
  ValidDate(Year2, Month2, Day2);

  if Year1 > Year2 then // 年大
  begin
    Result := 1
  end
  else if Year1 = Year2 then // 年等
  begin
    if Month1 > Month2 then  // 年等月大
    begin
      Result := 1
    end
    else if Month1 = Month2 then // 年等月等
    begin
      if Day1 > Day2 then // 年等月等日大
      begin
        Result := 1
      end
      else if Day1 = Day2 then // 年等月等日等
      begin
        Result := 0;
      end
      else  // 年等月等日小
      begin
        Result := -1;
      end;
    end
    else // 年等月小
    begin
      Result := -1;
    end;
  end
  else // 年小
  begin
    Result := -1;
  end;
end;

// 比较两个农历日期（包括闰月信息），1 >=< 2 分别返回 1、0、-1
function Compare2LunarDay(Year1, Month1, Day1: Integer; IsLeap1: Boolean;
  Year2, Month2, Day2: Integer; IsLeap2: Boolean): Integer;
begin
  if Year1 > Year2 then // 年大
  begin
    Result := 1
  end
  else if Year1 = Year2 then // 年等
  begin
    if Month1 > Month2 then  // 年等月大
    begin
      Result := 1
    end
    else if Month1 = Month2 then // 年等月等
    begin
      if IsLeap1 = IsLeap2 then // 闰也等
      begin
        if Day1 > Day2 then // 年等月等日大
        begin
          Result := 1
        end
        else if Day1 = Day2 then // 年等月等日等
        begin
          Result := 0;
        end
        else  // 年等月等日小
        begin
          Result := -1;
        end;
      end
      else if IsLeap1 and not IsLeap2 then // 闰某月大于某月
        Result := 1
      else
        Result := -1;
    end
    else // 年等月小
    begin
      Result := -1;
    end;
  end
  else // 年小
  begin
    Result := -1;
  end;
end;

// 取某日期到年初的天数，不考虑 1582 年 10 月的特殊情况
function GetDayFromYearBegin(AYear, AMonth, ADay: Integer): Integer;
const
  MonthAbsDays: array [Boolean] of TDayTable =
    ((0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
     (0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335));
begin
  Result := MonthAbsDays[GetIsLeapYear(AYear)][AMonth] + ADay;
end;

// 取某日期到年初的天数，小时、分、秒数折算入小数，不考虑 1582 年 10 月的特殊情况
function GetDayFromYearBegin(AYear, AMonth, ADay, AHour: Integer;
  AMinute, ASecond: Integer): Extended;
begin
  Result := GetDayFromYearBegin(AYear, AMonth, ADay);
  Result := Result + (AHour / 24.0) + (AMinute / 1440.0) + (ASecond / 86400.0);
end;

// 从距年首天数返回月和日数，年份用来判断是否是闰年，返回 False 表示不合法日期
function ExtractMonthDay(Days: Integer; AYear: Integer; out AMonth: Integer;
  out ADay: Integer): Boolean;
var
  I, Day: Integer;
begin
  if (Days <= 0) or (Days > 366) or ((Days > 365) and (not GetIsLeapYear(AYear))) then
  begin
    Result := False;
    AMonth := -1;
    ADay := -1;
    Exit;
  end;

  for I := 1 to 12 do
  begin
    Day := GetMonthDays(AYear, I);
    if Days > Day then
      Days := Days - Day
    else
    begin
      AMonth := I;
      Break;
    end;
  end;
  ADay := Floor(Days);
  Result := True;
end;

function GetBasicDays(AYear, AMonth, ADay: Integer): Integer;
var
  I: Integer;
begin
  if AYear > 0 then
    Result := (AYear - 1) * 365
  else
    Result := AYear * 365;

  for I := 1 to AMonth - 1 do
    Inc(Result, GetMonthDays(AYear, I));
  Inc(Result, ADay);
end;

function GetLeapDays(AYear, AMonth, ADay: Integer): Integer;
begin
  if AYear >= 0 then // 公元后
  begin
    if GetCalendarType(AYear, AMonth, ADay) in [ctJulian, ctInvalid] then
      Result := 0
    else
    begin
      // 1582.10.5/15 前的儒略历只有四年一闰，历法此日后调整为格里高利历
      Result := 10; // 被格里高利删去的 10 天

      if AYear > 1700 then // 修正算法简化版，从 1701 年的 11 起
      begin
        // 每一世纪累加一
        Inc(Result, 1 + ((AYear - 1701) div 100));
        // 但 400 整除的世纪不加
        Dec(Result, ((AYear - 1601) div 400));
      end;
    end;
    Result := ((AYear - 1) div 4) - Result; // 4 年一闰数
  end
  else // 公元前
  begin
    Result := - ((- AYear + 3) div 4);
  end;
end;

// 获得距公元原点的日数
function GetAllDays(AYear, AMonth, ADay: Integer): Integer;
begin
  Result := GetBasicDays(AYear, AMonth, ADay) + GetLeapDays(AYear, AMonth, ADay);
end;

// 获取等效标准天数，此概念系移植而来，似乎是距格里高利历元年元旦的天数
// 注意此处的格里高利历不包括删去的 10 天，因此等效标准日是连续的
function GetEquStandardDays(AYear, AMonth, ADay: Integer): Integer;
var
  AType: TCnCalendarType;
begin
  Result := 0;
  AType := GetCalendarType(AYear, AMonth, ADay);
  if AType = ctGregorian then
  begin
    Result := (AYear - 1) * 365 + ((AYear - 1) div 4) -((AYear - 1) div 100)
     + ((AYear - 1) div 400) + GetDayFromYearBegin(AYear, AMonth, ADay);
  end
  else if AType = ctJulian then
  begin
    { 为啥最后减 2？猜测公元 1 年到 1582 年，儒略历较格里高利历多闰了 12 天，
      (100, 200, 300, 500, 600, 700, 900, 1000, 1100, 1300, 1400, 1500)
      而格里高利只删去 10 天，所以留下了 2 天的差值。
      这说明，按格里高利历从 1582.10.4 往前倒推得的格里高利历元年元旦
      和实际公元元年元旦不是同一天。 }
    if AYear > 0 then
      Result := (AYear - 1) * 365 + ((AYear - 1) div 4)
        + GetDayFromYearBegin(AYear, AMonth, ADay) - 2
    else
      Result := (AYear - 1) * 365 + ((AYear) div 4) - 1     // 这里减一是因为 0 年也是闰年，多一个
        + GetDayFromYearBegin(AYear - 1, AMonth, ADay) - 2;
    // GetDayFromYearBegin 需要正经的非 0 公历年，但 AYear 传的是连续的所以要减一
  end;
end;

// 获得等效标准日数对应的某公历日，倒推而来
function GetDayFromEquStandardDays(EquDays: Integer;
  out AYear, AMonth, ADay: Integer): Boolean;
const
  D1 = 365;                 // 单个年            365
  D4 = D1 * 4 + 1;          // 四年加一闰日      1461
  D100 = D4 * 25 - 1;       // 百年不闰减一日    36524
  D400 = D100 * 4 + 1;      // 四百年又闰加一日  146101
  MORE_LEAP_DAYS: array[1..12] of Integer = // 儒略历多闰的 12 年的 2 月 29 对应的等效标准日
    (36217, 72742, 109267, 182317, 218842, 255367, 328417, 364942, 401467, 474517, 511042, 547567);
  // 也即公元 100, 200, 300, 500, 600, 700, 900, 1000, 1100, 1300, 1400, 1500 的 2 月 29 日是儒略历多闰出来的
var
  Diff: Integer;
  Y, M, D, I: Word;
  T: Integer;
  DayTable: PDayTable;
  IsJunlian229: Boolean;

  // 判断 0 连续的年份是否是儒略历里多出来的闰年，也就是逢非四的百年
  function IsJulianNotGregorianLeap(AYear: Integer): Boolean;
  begin
    Result := ((AYear mod 100) = 0) and ((AYear mod 400) <> 0);
  end;

begin
  Result := False;
  AYear := 0;
  AMonth := 0;
  ADay := 0;

  if EquDays < 0 then Exit;  // 暂不处理公元前的等效标准日

  IsJunlian229 := False;
  if EquDays <= 577735 then  // 如果是 1582.10.4 (577735) 及之前为儒略历，需要记录是否多闰的 2 月 29 日以及修正
  begin
    // 计算和 36217 也即 公元 100 年 2 月 29 日的距离
    Diff := EquDays - 36217;
    if Diff < 0 then
      Diff := -Diff;

    if (Diff mod 36525) = 0 then
    begin
      Diff := Diff div 36525; // 百年倍数
      Inc(Diff);
      if Diff mod 4 <> 0 then
        IsJunlian229 := True;   // 该数是儒略历里多闰出的 2 月 29日
    end;

    Diff := 0;
    for I := High(MORE_LEAP_DAYS) downto Low(MORE_LEAP_DAYS) do
    begin
      if EquDays > MORE_LEAP_DAYS[I] then
      begin
        Diff := I;
        Break;
      end;
    end;

    // Diff := EquDays div (365 * 100) - EquDays div (365 * 400);
    Dec(EquDays, 10);        // 格里高利删去的 10 天
    Inc(EquDays, 12 - Diff); // 补上多闰的 12 天中多闰的部分
  end;

  T := EquDays;
  Y := 1;
  while T >= D400 do         // 过了几个四百年又闰
  begin
    Dec(T, D400);
    Inc(Y, 400);             // 扣掉四百年整数倍
  end;

  I := T div D100;           // 过了几个百年不闰
  D := T mod D100;
  if I = 4 then              // 如果过了四个百年不闰
  begin
    Dec(I);
    Inc(D, D100);
  end;
  Inc(Y, I * 100);

  I := D div D4;             // 又过了几个四年一闰
  D := D mod D4;
  Inc(Y, I * 4);

  I := D div D1;             // 又过了几个整年
  D := D mod D1;
  if I = 4 then
  begin
    Dec(I);
    Inc(D, D1);
  end;
  Inc(Y, I);

  DayTable := @MonthDays[(Y mod 4 = 0) and ((Y mod 100 <> 0) or (Y mod 400 = 0))];
  // 注意不能用 GetIsLeapYear(Y) 这个儒略和格里高利的混合判断
  // 根据定义得用纯的格里高利历的闰年判断

  M := 1;
  if D > 0 then
  begin
    while True do
    begin
      I := DayTable^[M];

      if IsJunlian229 and (M = 2) then // 仅在特定的日子做修正
        I := 29;

      if D <= I then
        Break;
      Dec(D, I);
      Inc(M);
    end;
  end
  else
  begin
    // 如果 D 恰好整除成 0，说明 1 月 0 日就是上一年的 12 月 31 日
    Dec(Y);
    M := 12;
    D := 31;
  end;

  AYear := Y;
  AMonth := M;
  ADay := D;

  Result := True;
end;

function GetJulianDate(AYear, AMonth, ADay: Integer): Extended;
var
  A, Y, M: Integer;
begin
  ValidDate(AYear, AMonth, ADay);

  // 外界公元没有公元 0 年，但此处内部年份计算要连续，所以内部把公元前的年份数加一
  NonZeroYearToZeroYear(AYear);

  A := (14 - AMonth) div 12;
  Y := AYear + 4800 - A;
  M := AMonth + 12 * A - 3;

  if GetCalendarType(AYear, AMonth, ADay) = ctGregorian then
    Result := ADay + ((153 * M + 2) div 5) + 365 * Y + (Y div 4) - (Y div 100) + (Y div 400) - 32045
  else
    Result := ADay + ((153 * M + 2) div 5) + 365 * Y + (Y div 4) - 32083;
end;

function GetJulianDate(AYear, AMonth, ADay: Integer;
  AHour, AMinute, ASecond: Integer): Extended; overload;
begin
  Result := GetJulianDate(AYear, AMonth, ADay) - 0.5; // 得到 0 时的儒略日数
  Result := Result + (AHour * 3600 + AMinute * 60 + ASecond) / 86400; // 加上当日小数
end;

function GetModifiedJulianDate(AYear, AMonth, ADay: Integer): Extended;
begin
  Result := GetJulianDate(AYear, AMonth, ADay) - 2400000.5;
end;

function GetModifiedJulianDate(AYear, AMonth, ADay: Integer;
  AHour, AMinute, ASecond: Integer): Extended;
begin
  Result := GetJulianDate(AYear, AMonth, ADay, AHour, AMinute, ASecond) - 2400000.5;
end;

function GetDayFromJulianDate(JD: Extended; out AYear, AMonth, ADay: Integer): Boolean;
var
  A, B, C, D, E, M: Double;
begin
  // Jean Meeus 转换算法
  A := JD;
  if A < 2299161 then          // 判断是否在格里高利历启用前
    B := A
  else
  begin
    C := Trunc((A - 1867216.25)/36524.25); // 处理格里历置闰规则
    B := A + 1 + C - Trunc(C / 4);         // 补偿历法变更误差
  end;

  D := B + 1524;                  // 调整历元至公元前 4716 年 3 月 1 日
  C := Trunc((D - 122.1)/365.25); // 计算年份基数
  E := Trunc(365.25 * C);         // 年积日
  M := Trunc((D - E)/30.6001);    // 计算月份基数

  // 计算具体日期
  ADay := Trunc(D - E - Trunc(30.6001 * M)); // 日数计算
  AMonth := Trunc(M - 1);                    // 处理月份偏移
  if AMonth > 12 then
    AMonth := AMonth - 12;        // 调整 12 月后的月份

  AYear := Trunc(C - 4715);       // 年份基数转换
  if AMonth > 2 then              // 处理 1-2 月属于前一年的情况
    Dec(AYear);

  // 计算得到的是连续的年份，负值转换为无公元 0 年的年份
  ZeroYearToNonZeroYear(AYear);
  Result := GetDateIsValid(AYear, AMonth, ADay);
end;

function GetDayFromModifiedJulianDate(MJD: Extended; out AYear, AMonth, ADay: Integer): Boolean;
var
  JD: Extended;
begin
  // 约化儒略日转标准儒略日
  JD := MJD + 2400000.5;
  Result := GetDayFromJulianDate(JD, AYear, AMonth, ADay);
end;

// 获得某日期是星期几，0-6
function GetWeek(const AValue: TDateTime): Integer; overload;
var
  Year, Month, Day: Word;
begin
  DecodeDate(AValue, Year, Month, Day);
  // -2 源于公元 1 年 1 月 2 日才是星期天
  Result := (GetAllDays(Year, Month, Day) - 2) mod 7;
  if Result < 0 then
    Inc(Result, 7);
end;

// 获得某日期是星期几，0-6
function GetWeek(AYear, AMonth, ADay: Integer): Integer; overload;
begin
  // -2 源于公元 1 年 1 月 2 日才是星期天
  Result := (GetAllDays(AYear, AMonth, ADay) - 2) mod 7;
  if Result < 0 then
    Inc(Result, 7);
end;

// 从数字获得星期名，不包括星期二字, 0-6 对应 日到六
function GetWeekFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if AValue in [0..6] then
    Result := SCnWeekNumberArray[AValue];
end;

// ============================= 节气粗略算法 ==================================

// 基本算法之获得某公历年内的第 N 个节气距年初的天数，1-24，对应小寒到冬至。年数不能为 0
// 考虑了 1582 年之前公历有十天偏差的情况，公历年不能为 0
// 注；因不够精确已基本弃用
function _GetJieQiDayTimeFromYear(AYear, N: Integer): Extended; {$IFDEF SUPPORT_DEPRECATED} deprecated; {$ENDIF}
var
  JuD, Tht, YrD, ShuoD: Extended;
begin
  { 由于进动章动等造成的岁差的影响，太阳两次通过各个定气点的时间并不是一精确回归年
    所以没法以 365.2422 为周期来直接计算各个节气的时刻。下面这个公式属移植而来。
    返回的天数是小数，可折算成月日时分秒，但精度不够高，误差在 10 分钟左右，秒无意义。}

  // 对没有公元 0 年的调整，以变成连续的公历年供下文进行计算
  NonZeroYearToZeroYear(AYear);

  JuD := AYear * (365.2423112 - 6.4e-14 * (AYear-100) * (AYear - 100)
    - 3.047e-8 * (AYear-100)) + 15.218427 * N + 1721050.71301;
  Tht := 3e-4 * AYear - 0.372781384 - 0.2617913325 * N;
  YrD := (1.945 * Sin(Tht) - 0.01206 * Sin(2 * Tht)) * (1.048994 - 2.583e-5 * AYear);
  ShuoD := -18e-4 * Sin(2.313908653 * AYear - 0.439822951 - 3.0443 * N);
  Result := JuD + YrD + ShuoD - GetEquStandardDays(AYear, 1, 0) - 1721425; // 定气
  // (juD - GetEquStandardDays(AYear, 1, 0) - 1721425); 平气
  if AYear <= 1582 then // 1582 年被删掉了 10 天，要加回来
    Result := Result + 10;
end;

// =========================== 节气精确算法开始 ================================

const
  CN_PI: Extended = 3.1415926535897932384626;

  RAD = 180 * 3600 / PI;   // 每弧度的角秒数

  J2000 = 2451545;

  CN_DT_AT: array[0..101] of Extended = ( // TD - UT1 五个一组的计算表
    -4000,108371.7,-13036.80,392.000, 0.0000,
     -500, 17201.0,  -627.82, 16.170,-0.3413,
     -150, 12200.6,  -346.41,  5.403,-0.1593,
      150,  9113.8,  -328.13, -1.647, 0.0377,
      500,  5707.5,  -391.41,  0.915, 0.3145,
      900,  2203.4,  -283.45, 13.034,-0.1778,
     1300,   490.1,   -57.35,  2.085,-0.0072,
     1600,   120.0,    -9.81, -1.532, 0.1403,
     1700,    10.2,    -0.91,  0.510,-0.0370,
     1800,    13.4,    -0.72,  0.202,-0.0193,
     1830,     7.8,    -1.81,  0.416,-0.0247,
     1860,     8.3,    -0.13, -0.406, 0.0292,
     1880,    -5.4,     0.32, -0.183, 0.0173,
     1900,    -2.3,     2.06,  0.169,-0.0135,
     1920,    21.2,     1.69, -0.304, 0.0167,
     1940,    24.2,     1.22, -0.064, 0.0031,
     1960,    33.2,     0.51,  0.231,-0.0109,
     1980,    51.0,     1.29, -0.026, 0.0032,
     2000,    63.87,    0.1,   0,     0,
     2005,    64.7,     0.4,   0,     0,   //一次项记为x,则 10x=0.4秒/年*(2015-2005),解得x=0.4
     2015,    69
    );

  nutB: array[0..49] of Extended = ( // 中精度章动计算表
    2.1824,  -33.75705, 36e-6,-1720,920,
    3.5069, 1256.66393, 11e-6,-132, 57,
    1.3375,16799.4182, -51e-6, -23, 10,
    4.3649,  -67.5141,  72e-6,  21, -9,
    0.04,   -628.302,   0,     -14,  0,
    2.36,   8328.691,   0,       7,  0,
    3.46,   1884.966,   0,      -5,  2,
    5.44,  16833.175,   0,      -4,  2,
    3.69,  25128.110,   0,      -3,  0,
    3.55,    628.362,   0,       2,  0
  );

  XL0: array[0..2665] of Extended = (
  {* 八大行星星历数据表中的地球部分及数据表的计算
    Dear 精度：J2000+-4千年 黄经0.1角秒 黄纬0.1角秒 距离0.1AU/10^6 }
    10000000000,// A 的倍率
    20,578,920,1100,1124,1136,1148,1217,1226,1229,1229,1229,1229,1937,2363,2618,2633,2660,2666,// 位置索引表
    {L0}
    17534704567,0.00000000000,0.00000000000,334165646,4.669256804,6283.075849991,3489428,4.6261024,12566.1517000,349706,
    2.744118,5753.384885,341757,2.828866,3.523118,313590,3.627670,77713.771468,267622,4.418084,7860.419392,234269,6.135162,
    3930.209696,132429,0.742464,11506.769770,127317,2.037097,529.690965,119917,1.109629,1577.343542,99025,5.23268,5884.92685,
    90186,2.04505,26.29832,85722,3.50849,398.14900,77979,1.17883,5223.69392,75314,2.53339,5507.55324,50526,4.58293,18849.22755,
    49238,4.20507,775.52261,35666,2.91954,0.06731,31709,5.84902,11790.62909,28413,1.89869,796.29801,27104,0.31489,10977.07880,
    24281,0.34481,5486.77784,20616,4.80647,2544.31442,20539,1.86948,5573.14280,20226,2.45768,6069.77675,15552,0.83306,
    213.29910,13221,3.41118,2942.46342,12618,1.08303,20.77540,11513,0.64545,0.98032,10285,0.63600,4694.00295,10190,0.97569,
    15720.83878,10172,4.26680,7.11355,9921,6.2099,2146.1654,9761,0.6810,155.4204,8580,5.9832,161000.6857,8513,1.2987,6275.9623,
    8471,3.6708,71430.6956,7964,1.8079,17260.1547,7876,3.0370,12036.4607,7465,1.7551,5088.6288,7387,3.5032,3154.6871,7355,
    4.6793,801.8209,6963,0.8330,9437.7629,6245,3.9776,8827.3903,6115,1.8184,7084.8968,5696,2.7843,6286.5990,5612,4.3869,
    14143.4952,5558,3.4701,6279.5527,5199,0.1891,12139.5535,5161,1.3328,1748.0164,5115,0.2831,5856.4777,4900,0.4874,1194.4470,
    4104,5.3682,8429.2413,4094,2.3985,19651.0485,3920,6.1683,10447.3878,3677,6.0413,10213.2855,3660,2.5696,1059.3819,3595,
    1.7088,2352.8662,3557,1.7760,6812.7668,3329,0.5931,17789.8456,3041,0.4429,83996.8473,3005,2.7398,1349.8674,2535,3.1647,
    4690.4798,2474,0.2148,3.5904,2366,0.4847,8031.0923,2357,2.0653,3340.6124,2282,5.2220,4705.7323,2189,5.5559,553.5694,2142,
    1.4256,16730.4637,2109,4.1483,951.7184,2030,0.3713,283.8593,1992,5.2221,12168.0027,1986,5.7747,6309.3742,1912,3.8222,
    23581.2582,1889,5.3863,149854.4001,1790,2.2149,13367.9726,1748,4.5605,135.0651,1622,5.9884,11769.8537,1508,4.1957,
    6256.7775,1442,4.1932,242.7286,1435,3.7236,38.0277,1397,4.4014,6681.2249,1362,1.8893,7632.9433,1250,1.1305,5.5229,1205,
    2.6223,955.5997,1200,1.0035,632.7837,1129,0.1774,4164.3120,1083,0.3273,103.0928,1052,0.9387,11926.2544,1050,5.3591,
    1592.5960,1033,6.1998,6438.4962,1001,6.0291,5746.2713,980,0.999,11371.705,980,5.244,27511.468,938,2.624,5760.498,923,0.483,
    522.577,922,4.571,4292.331,905,5.337,6386.169,862,4.165,7058.598,841,3.299,7234.794,836,4.539,25132.303,813,6.112,4732.031,
    812,6.271,426.598,801,5.821,28.449,787,0.996,5643.179,776,2.957,23013.540,769,3.121,7238.676,758,3.974,11499.656,735,4.386,
    316.392,731,0.607,11513.883,719,3.998,74.782,706,0.323,263.084,676,5.911,90955.552,663,3.665,17298.182,653,5.791,18073.705,
    630,4.717,6836.645,615,1.458,233141.314,612,1.075,19804.827,596,3.321,6283.009,596,2.876,6283.143,555,2.452,12352.853,541,
    5.392,419.485,531,0.382,31441.678,519,4.065,6208.294,513,2.361,10973.556,494,5.737,9917.697,450,3.272,11015.106,449,3.653,
    206.186,447,2.064,7079.374,435,4.423,5216.580,421,1.906,245.832,413,0.921,3738.761,402,0.840,20.355,387,1.826,11856.219,
    379,2.344,3.881,374,2.954,3128.389,370,5.031,536.805,365,1.018,16200.773,365,1.083,88860.057,352,5.978,3894.182,352,2.056,
    244287.600,351,3.713,6290.189,340,1.106,14712.317,339,0.978,8635.942,339,3.202,5120.601,333,0.837,6496.375,325,3.479,
    6133.513,316,5.089,21228.392,316,1.328,10873.986,309,3.646,10.637,303,1.802,35371.887,296,3.397,9225.539,288,6.026,
    154717.610,281,2.585,14314.168,262,3.856,266.607,262,2.579,22483.849,257,1.561,23543.231,255,3.949,1990.745,251,3.744,
    10575.407,240,1.161,10984.192,238,0.106,7.046,236,4.272,6040.347,234,3.577,10969.965,211,3.714,65147.620,210,0.754,
    13521.751,207,4.228,5650.292,202,0.814,170.673,201,4.629,6037.244,200,0.381,6172.870,199,3.933,6206.810,199,5.197,6262.300,
    197,1.046,18209.330,195,1.070,5230.807,195,4.869,36.028,194,4.313,6244.943,192,1.229,709.933,192,5.595,6282.096,192,0.602,
    6284.056,189,3.744,23.878,188,1.904,15.252,188,0.867,22003.915,182,3.681,15110.466,181,0.491,1.484,179,3.222,39302.097,179,
    1.259,12559.038,
    {L1}
    62833196674749,0.000000000000,0.000000000000,20605886,2.67823456,6283.07584999,430343,2.635127,12566.151700,42526,1.59047,
    3.52312,11926,5.79557,26.29832,10898,2.96618,1577.34354,9348,2.5921,18849.2275,7212,1.1385,529.6910,6777,1.8747,398.1490,
    6733,4.4092,5507.5532,5903,2.8880,5223.6939,5598,2.1747,155.4204,4541,0.3980,796.2980,3637,0.4662,775.5226,2896,2.6471,
    7.1135,2084,5.3414,0.9803,1910,1.8463,5486.7778,1851,4.9686,213.2991,1729,2.9912,6275.9623,1623,0.0322,2544.3144,1583,
    1.4305,2146.1654,1462,1.2053,10977.0788,1246,2.8343,1748.0164,1188,3.2580,5088.6288,1181,5.2738,1194.4470,1151,2.0750,
    4694.0030,1064,0.7661,553.5694,997,1.303,6286.599,972,4.239,1349.867,945,2.700,242.729,858,5.645,951.718,758,5.301,
    2352.866,639,2.650,9437.763,610,4.666,4690.480,583,1.766,1059.382,531,0.909,3154.687,522,5.661,71430.696,520,1.854,801.821,
    504,1.425,6438.496,433,0.241,6812.767,426,0.774,10447.388,413,5.240,7084.897,374,2.001,8031.092,356,2.429,14143.495,350,
    4.800,6279.553,337,0.888,12036.461,337,3.862,1592.596,325,3.400,7632.943,322,0.616,8429.241,318,3.188,4705.732,297,6.070,
    4292.331,295,1.431,5746.271,290,2.325,20.355,275,0.935,5760.498,270,4.804,7234.794,253,6.223,6836.645,228,5.003,17789.846,
    225,5.672,11499.656,215,5.202,11513.883,208,3.955,10213.286,208,2.268,522.577,206,2.224,5856.478,206,2.550,25132.303,203,
    0.910,6256.778,189,0.532,3340.612,188,4.735,83996.847,179,1.474,4164.312,178,3.025,5.523,177,3.026,5753.385,159,4.637,
    3.286,157,6.124,5216.580,155,3.077,6681.225,154,4.200,13367.973,143,1.191,3894.182,138,3.093,135.065,136,4.245,426.598,134,
    5.765,6040.347,128,3.085,5643.179,127,2.092,6290.189,125,3.077,11926.254,125,3.445,536.805,114,3.244,12168.003,112,2.318,
    16730.464,111,3.901,11506.770,111,5.320,23.878,105,3.750,7860.419,103,2.447,1990.745,96,0.82,3.88,96,4.08,6127.66,91,5.42,
    206.19,91,0.42,7079.37,88,5.17,11790.63,81,0.34,9917.70,80,3.89,10973.56,78,2.40,1589.07,78,2.58,11371.70,77,3.98,955.60,
    77,3.36,36.03,76,1.30,103.09,75,5.18,10969.97,75,4.96,6496.37,73,5.21,38.03,72,2.65,6309.37,70,5.61,3738.76,69,2.60,
    3496.03,69,0.39,15.25,69,2.78,20.78,65,1.13,7058.60,64,4.28,28.45,61,5.63,10984.19,60,0.73,419.48,60,5.28,10575.41,58,5.55,
    17298.18,58,3.19,4732.03,
    {L2}
    5291887,0.0000000,0.0000000,871984,1.072097,6283.075850,30913,0.86729,12566.15170,2734,0.0530,3.5231,1633,5.1883,26.2983,
    1575,3.6846,155.4204,954,0.757,18849.228,894,2.057,77713.771,695,0.827,775.523,506,4.663,1577.344,406,1.031,7.114,381,
    3.441,5573.143,346,5.141,796.298,317,6.053,5507.553,302,1.192,242.729,289,6.117,529.691,271,0.306,398.149,254,2.280,
    553.569,237,4.381,5223.694,208,3.754,0.980,168,0.902,951.718,153,5.759,1349.867,145,4.364,1748.016,134,3.721,1194.447,125,
    2.948,6438.496,122,2.973,2146.165,110,1.271,161000.686,104,0.604,3154.687,100,5.986,6286.599,92,4.80,5088.63,89,5.23,
    7084.90,83,3.31,213.30,76,3.42,5486.78,71,6.19,4690.48,68,3.43,4694.00,65,1.60,2544.31,64,1.98,801.82,61,2.48,10977.08,50,
    1.44,6836.65,49,2.34,1592.60,46,1.31,4292.33,46,3.81,149854.40,43,0.04,7234.79,40,4.94,7632.94,39,1.57,71430.70,38,3.17,
    6309.37,35,0.99,6040.35,35,0.67,1059.38,31,3.18,2352.87,31,3.55,8031.09,30,1.92,10447.39,30,2.52,6127.66,28,4.42,9437.76,
    28,2.71,3894.18,27,0.67,25132.30,26,5.27,6812.77,25,0.55,6279.55,23,1.38,4705.73,22,0.64,6256.78,20,6.07,640.88,
    {L3}
    28923,5.84384,6283.07585,3496,0.0000,0.0000,1682,5.4877,12566.1517,296,5.196,155.420,129,4.722,3.523,71,5.30,18849.23,64,
    5.97,242.73,40,3.79,553.57,
    {L4}
    11408,3.14159,0.00000,772,4.134,6283.076,77,3.84,12566.15,42,0.42,155.42,
    {L5}
    88,3.14,0.00,17,2.77,6283.08,5,2.01,155.42,3,2.21,12566.15,
    {B0}
    27962,3.19870,84334.66158,10164,5.42249,5507.55324,8045,3.8801,5223.6939,4381,3.7044,2352.8662,3193,4.0003,1577.3435,2272,
    3.9847,1047.7473,1814,4.9837,6283.0758,1639,3.5646,5856.4777,1444,3.7028,9437.7629,1430,3.4112,10213.2855,1125,4.8282,
    14143.4952,1090,2.0857,6812.7668,1037,4.0566,71092.8814,971,3.473,4694.003,915,1.142,6620.890,878,4.440,5753.385,837,
    4.993,7084.897,770,5.554,167621.576,719,3.602,529.691,692,4.326,6275.962,558,4.410,7860.419,529,2.484,4705.732,521,6.250,
    18073.705,
    {B1}
    903,3.897,5507.553,618,1.730,5223.694,380,5.244,2352.866,
    {B2}
    166,1.627,84334.662,

    {R0}
    10001398880,0.00000000000,0.00000000000,167069963,3.098463508,6283.075849991,1395602,3.0552461,12566.1517000,308372,
    5.198467,77713.771468,162846,1.173877,5753.384885,157557,2.846852,7860.419392,92480,5.45292,11506.76977,54244,4.56409,
    3930.20970,47211,3.66100,5884.92685,34598,0.96369,5507.55324,32878,5.89984,5223.69392,30678,0.29867,5573.14280,24319,
    4.27350,11790.62909,21183,5.84715,1577.34354,18575,5.02194,10977.07880,17484,3.01194,18849.22755,10984,5.05511,5486.77784,
    9832,0.8868,6069.7768,8650,5.6896,15720.8388,8583,1.2708,161000.6857,6490,0.2725,17260.1547,6292,0.9218,529.6910,5706,
    2.0137,83996.8473,5574,5.2416,71430.6956,4938,3.2450,2544.3144,4696,2.5781,775.5226,4466,5.5372,9437.7629,4252,6.0111,
    6275.9623,3897,5.3607,4694.0030,3825,2.3926,8827.3903,3749,0.8295,19651.0485,3696,4.9011,12139.5535,3566,1.6747,12036.4607,
    3454,1.8427,2942.4634,3319,0.2437,7084.8968,3192,0.1837,5088.6288,3185,1.7778,398.1490,2846,1.2134,6286.5990,2779,1.8993,
    6279.5527,2628,4.5890,10447.3878,2460,3.7866,8429.2413,2393,4.9960,5856.4777,2359,0.2687,796.2980,2329,2.8078,14143.4952,
    2210,1.9500,3154.6871,2035,4.6527,2146.1654,1951,5.3823,2352.8662,1883,0.6731,149854.4001,1833,2.2535,23581.2582,1796,
    0.1987,6812.7668,1731,6.1520,16730.4637,1717,4.4332,10213.2855,1619,5.2316,17789.8456,1381,5.1896,8031.0923,1364,3.6852,
    4705.7323,1314,0.6529,13367.9726,1041,4.3329,11769.8537,1017,1.5939,4690.4798,998,4.201,6309.374,966,3.676,27511.468,874,
    6.064,1748.016,779,3.674,12168.003,771,0.312,7632.943,756,2.626,6256.778,746,5.648,11926.254,693,2.924,6681.225,680,1.423,
    23013.540,674,0.563,3340.612,663,5.661,11371.705,659,3.136,801.821,648,2.650,19804.827,615,3.029,233141.314,612,5.134,
    1194.447,563,4.341,90955.552,552,2.091,17298.182,534,5.100,31441.678,531,2.407,11499.656,523,4.624,6438.496,513,5.324,
    11513.883,477,0.256,11856.219,461,1.722,7234.794,458,3.766,6386.169,458,4.466,5746.271,423,1.055,5760.498,422,1.557,
    7238.676,415,2.599,7058.598,401,3.030,1059.382,397,1.201,1349.867,379,4.907,4164.312,360,5.707,5643.179,352,3.626,
    244287.600,348,0.761,10973.556,342,3.001,4292.331,336,4.546,4732.031,334,3.138,6836.645,324,4.164,9917.697,316,1.691,
    11015.106,307,0.238,35371.887,298,1.306,6283.143,298,1.750,6283.009,293,5.738,16200.773,286,5.928,14712.317,281,3.515,
    21228.392,280,5.663,8635.942,277,0.513,26.298,268,4.207,18073.705,266,0.900,12352.853,260,2.962,25132.303,255,2.477,
    6208.294,242,2.800,709.933,231,1.054,22483.849,229,1.070,14314.168,216,1.314,154717.610,215,6.038,10873.986,200,0.561,
    7079.374,198,2.614,951.718,197,4.369,167283.762,186,2.861,5216.580,183,1.660,39302.097,183,5.912,3738.761,175,2.145,
    6290.189,173,2.168,10575.407,171,3.702,1592.596,171,1.343,3128.389,164,5.550,6496.375,164,5.856,10984.192,161,1.998,
    10969.965,161,1.909,6133.513,157,4.955,25158.602,154,6.216,23543.231,153,5.357,13521.751,150,5.770,18209.330,150,5.439,
    155.420,139,1.778,9225.539,139,1.626,5120.601,128,2.460,13916.019,123,0.717,143571.324,122,2.654,88860.057,121,4.414,
    3894.182,121,1.192,3.523,120,4.030,553.569,119,1.513,17654.781,117,3.117,14945.316,113,2.698,6040.347,110,3.085,43232.307,
    109,0.998,955.600,108,2.939,17256.632,107,5.285,65147.620,103,0.139,11712.955,103,5.850,213.299,102,3.046,6037.244,101,
    2.842,8662.240,100,3.626,6262.300,98,2.36,6206.81,98,5.11,6172.87,98,2.00,15110.47,97,2.67,5650.29,97,2.75,6244.94,96,4.02,
    6282.10,96,5.31,6284.06,92,0.10,29088.81,85,3.26,20426.57,84,2.60,28766.92,81,3.58,10177.26,80,5.81,5230.81,78,2.53,
    16496.36,77,4.06,6127.66,73,0.04,5481.25,72,5.96,12559.04,72,5.92,4136.91,71,5.49,22003.91,70,3.41,7.11,69,0.62,11403.68,
    69,3.90,1589.07,69,1.96,12416.59,69,4.51,426.60,67,1.61,11087.29,66,4.50,47162.52,66,5.08,283.86,66,4.32,16858.48,65,1.04,
    6062.66,64,1.59,18319.54,63,5.70,45892.73,63,4.60,66567.49,63,3.82,13517.87,62,2.62,11190.38,61,1.54,33019.02,60,5.58,
    10344.30,60,5.38,316428.23,60,5.78,632.78,59,6.12,9623.69,57,0.16,17267.27,57,3.86,6076.89,57,1.98,7668.64,56,4.78,
    20199.09,55,4.56,18875.53,55,3.51,17253.04,54,3.07,226858.24,54,4.83,18422.63,53,5.02,12132.44,52,3.63,5333.90,52,0.97,
    155427.54,51,3.36,20597.24,50,0.99,11609.86,50,2.21,1990.75,48,1.62,12146.67,48,1.17,12569.67,47,4.62,5436.99,47,1.81,
    12562.63,47,0.59,21954.16,47,0.76,7342.46,46,0.27,4590.91,46,3.77,156137.48,45,5.66,10454.50,44,5.84,3496.03,43,0.24,
    17996.03,41,5.93,51092.73,41,4.21,12592.45,40,5.14,1551.05,40,5.28,15671.08,39,3.69,18052.93,39,4.94,24356.78,38,2.72,
    11933.37,38,5.23,7477.52,38,4.99,9779.11,37,3.70,9388.01,37,4.44,4535.06,36,2.16,28237.23,36,2.54,242.73,36,0.22,5429.88,
    35,6.15,19800.95,35,2.92,36949.23,34,5.63,2379.16,34,5.73,16460.33,34,5.11,5849.36,33,6.19,6268.85,
    {R1}
    10301861,1.10748970,6283.07584999,172124,1.064423,12566.151700,70222,3.14159,0.00000,3235,1.0217,18849.2275,3080,2.8435,
    5507.5532,2497,1.3191,5223.6939,1849,1.4243,1577.3435,1008,5.9138,10977.0788,865,1.420,6275.962,863,0.271,5486.778,507,
    1.686,5088.629,499,6.014,6286.599,467,5.987,529.691,440,0.518,4694.003,410,1.084,9437.763,387,4.750,2544.314,375,5.071,
    796.298,352,0.023,83996.847,344,0.949,71430.696,341,5.412,775.523,322,6.156,2146.165,286,5.484,10447.388,284,3.420,
    2352.866,255,6.132,6438.496,252,0.243,398.149,243,3.092,4690.480,225,3.689,7084.897,220,4.952,6812.767,219,0.420,8031.092,
    209,1.282,1748.016,193,5.314,8429.241,185,1.820,7632.943,175,3.229,6279.553,173,1.537,4705.732,158,4.097,11499.656,158,
    5.539,3154.687,150,3.633,11513.883,148,3.222,7234.794,147,3.653,1194.447,144,0.817,14143.495,135,6.151,5746.271,134,4.644,
    6836.645,128,2.693,1349.867,123,5.650,5760.498,118,2.577,13367.973,113,3.357,17789.846,110,4.497,4292.331,108,5.828,
    12036.461,102,5.621,6256.778,99,1.14,1059.38,98,0.66,5856.48,93,2.32,10213.29,92,0.77,16730.46,88,1.50,11926.25,86,1.42,
    5753.38,85,0.66,155.42,81,1.64,6681.22,80,4.11,951.72,66,4.55,5216.58,65,0.98,25132.30,64,4.19,6040.35,64,0.52,6290.19,63,
    1.51,5643.18,59,6.18,4164.31,57,2.30,10973.56,55,2.32,11506.77,55,2.20,1592.60,55,5.27,3340.61,54,5.54,553.57,53,5.04,
    9917.70,53,0.92,11371.70,52,3.98,17298.18,52,3.60,10969.97,49,5.91,3894.18,49,2.51,6127.66,48,1.67,12168.00,46,0.31,801.82,
    42,3.70,10575.41,42,4.05,10984.19,40,2.17,7860.42,40,4.17,26.30,38,5.82,7058.60,37,3.39,6496.37,36,1.08,6309.37,36,5.34,
    7079.37,34,3.62,11790.63,32,0.32,16200.77,31,4.24,3738.76,29,4.55,11856.22,29,1.26,8635.94,27,3.45,5884.93,26,5.08,
    10177.26,26,5.38,21228.39,24,2.26,11712.96,24,1.05,242.73,24,5.59,6069.78,23,3.63,6284.06,23,1.64,4732.03,22,3.46,213.30,
    21,1.05,3496.03,21,3.92,13916.02,21,4.01,5230.81,20,5.16,12352.85,20,0.69,1990.75,19,2.73,6062.66,19,5.01,11015.11,18,6.04,
    6283.01,18,2.85,7238.68,18,5.60,6283.14,18,5.16,17253.04,18,2.54,14314.17,17,1.58,7.11,17,0.98,3930.21,17,4.75,17267.27,16,
    2.19,6076.89,16,2.19,18073.70,16,6.12,3.52,16,4.61,9623.69,16,3.40,16496.36,15,0.19,9779.11,15,5.30,13517.87,15,4.26,
    3128.39,15,0.81,709.93,14,0.50,25158.60,14,4.38,4136.91,13,0.98,65147.62,13,3.31,154717.61,13,2.11,1589.07,13,1.92,
    22483.85,12,6.03,9225.54,12,1.53,12559.04,12,5.82,6282.10,12,5.61,5642.20,12,2.38,167283.76,12,0.39,12132.44,12,3.98,
    4686.89,12,5.81,12569.67,12,0.56,5849.36,11,0.45,6172.87,11,5.80,16858.48,11,6.22,12146.67,11,2.27,5429.88,
    {R2}
    435939,5.784551,6283.075850,12363,5.57935,12566.15170,1234,3.1416,0.0000,879,3.628,77713.771,569,1.870,5573.143,330,5.470,
    18849.228,147,4.480,5507.553,110,2.842,161000.686,101,2.815,5223.694,85,3.11,1577.34,65,5.47,775.52,61,1.38,6438.50,50,
    4.42,6286.60,47,3.66,7084.90,46,5.39,149854.40,42,0.90,10977.08,40,3.20,5088.63,35,1.81,5486.78,32,5.35,3154.69,30,3.52,
    796.30,29,4.62,4690.48,28,1.84,4694.00,27,3.14,71430.70,27,6.17,6836.65,26,1.42,2146.17,25,2.81,1748.02,24,2.18,155.42,23,
    4.76,7234.79,21,3.38,7632.94,21,0.22,4705.73,20,4.22,1349.87,20,2.01,1194.45,20,4.58,529.69,19,1.59,6309.37,18,5.70,
    6040.35,18,6.03,4292.33,17,2.90,9437.76,17,2.00,8031.09,17,5.78,83996.85,16,0.05,2544.31,15,0.95,6127.66,14,0.36,10447.39,
    14,1.48,2352.87,13,0.77,553.57,13,5.48,951.72,13,5.27,6279.55,13,3.76,6812.77,11,5.41,6256.78,10,0.68,1592.60,10,4.95,
    398.15,10,1.15,3894.18,10,5.20,244287.60,10,1.94,11856.22,9,5.39,25132.30,8,6.18,1059.38,8,0.69,8429.24,8,5.85,242.73,7,
    5.26,14143.50,7,0.52,801.82,6,2.24,8635.94,6,4.00,13367.97,6,2.77,90955.55,6,5.17,7058.60,5,1.46,233141.31,5,4.13,7860.42,
    5,3.91,26.30,5,3.89,12036.46,5,5.58,6290.19,5,5.54,1990.75,5,0.83,11506.77,5,6.22,6681.22,4,5.26,10575.41,4,1.91,7477.52,4,
    0.43,10213.29,4,1.09,709.93,4,5.09,11015.11,4,4.22,88860.06,4,3.57,7079.37,4,1.98,6284.06,4,3.93,10973.56,4,6.18,9917.70,4,
    0.36,10177.26,4,2.75,3738.76,4,3.33,5643.18,4,5.36,25158.60,
    {R3}
    14459,4.27319,6283.07585,673,3.917,12566.152,77,0.00,0.00,25,3.73,18849.23,4,2.80,6286.60,
    {R4}
    386,2.564,6283.076,31,2.27,12566.15,5,3.44,5573.14,2,2.05,18849.23,1,2.06,77713.77,1,4.41,161000.69,1,3.82,149854.40,1,
    4.08,6127.66,1,5.26,6438.50,
    {R5}
    9,1.22,6283.08,1,0.66,12566.15
  );

// 根据年份和加速度进行二次曲线外推
function DiffTimeExt(Y, JSD: Extended): Extended;
var
  DY: Extended;
begin
  DY := (Y - 1820) / 100;
  Result := JSD * DY * DY - 20;
end;

// 计算世界时与原子时之差，传入年
function CalcDiffTime(Y: Extended): Extended;
var
  Y0, T0, V, DV, Acc, T1, T2, T3: Extended;
  I: Integer;
begin
  Y0 := CN_DT_AT[High(CN_DT_AT) - 1]; // 表中最后一年
  T0 := CN_DT_AT[High(CN_DT_AT)];     // 表中最后一年的 deltatT
  if Y >= Y0 then
  begin
    Acc := 31; // y0 年之后的加速度估计。瑞士星历表 31，NASA 网站 32，skmap 的 29
    if Y > Y0 + 100 then
    begin
      Result := DiffTimeExt(Y, Acc);
      Exit;
    end;
    V := DiffTimeExt(Y, Acc);         // 二次曲线外推
    DV := DiffTimeExt(Y0, Acc) - T0;  // ye 年的二次外推与 te 的差
    Result := V - DV * (Y0 + 100 - Y) / 100;
    Exit;
  end;

  I := 0;
  while I < High(CN_DT_AT) do
  begin
    if Y < CN_DT_AT[I + 5] then
      Break;
    I := I + 5;
  end;

  T1 := (Y - CN_DT_AT[I]) / (CN_DT_AT[I + 5] - CN_DT_AT[I]) * 10;
  T2 := T1 * T1;
  T3 := T2 * T1;
  Result := CN_DT_AT[I + 1] + CN_DT_AT[I + 2] * T1 +CN_DT_AT[I + 3] * T2
    + CN_DT_AT[I + 4] * T3;
end;

// 传入儒略日（J2000 起算），计算 TD-UT（单位：日）
function dt_T(T: Extended): Extended;
begin
  Result := CalcDiffTime(T / 365.2425 + 2000) / 86400.0;
end;

// 太阳光行差，T 是世纪数
function SolarAberration(T: Extended): Extended;
var
  V, E: Extended;
begin
  V := -0.043126 + 628.301955 * T - 0.000002732 * T * T;     // 平近点角
  E := 0.016708634 - 0.000042037 * T - 0.0000001267 * T * T;
  Result := (-20.49552 * (1 + E * Cos(V))) / RAD;            // 黄经光行差
end;

// 黄经章动计算
function EclipticLongitudeNutation(T: Extended): Extended;
var
  I: Integer;
  A, T2, DL: Extended;
begin
  T2 := T * T;
  DL := 0;

  I := 0;
  while I < High(nutB) do
  begin
    if I = 0 then
      A := -1.742 * T
    else
      A := 0;

    DL := DL + (nutB[I + 3] + A) * Sin(nutB[I] + nutB[I + 1] * T + nutB[I + 2] * T2);
    I := I + 5;
  end;
  Result := DL / (100 * RAD);
end;

// 星历表计算，t 儒略世纪数，n 计算项数
function CalcXingLi0(t, n: Extended): Extended;
var
  v, tn, c, NB, t2, t3: Extended;
  I, J, pn, n1, n2, n0, NZ: Integer;
begin
  t := t / 10;
  v := 0;
  tn := 1;
  pn := 1;

  NZ := Trunc(XL0[pn + 1] - XL0[pn]);

  for I := 0 to 5 do
  begin
    n1 := Trunc(XL0[pn + I]);
    n2 := Trunc(XL0[pn + I + 1]);
    n0 := n2 - n1;

    if n0 = 0 then
      Continue;

    if n < 0 then
      NB := n2
    else
    begin
      NB := Trunc(3 * n * n0 / NZ + 0.5) + n1;
      if I <> 0 then
        NB := NB + 3;
      if NB > n2 then
        NB := n2;
    end;

    c := 0;
    J := n1;
    while J < NB do
    begin
      c := c + XL0[J] * Cos(XL0[J + 1] + t * XL0[J + 2]);
      J := J + 3;
    end;

    v := v + c * tn;
    tn := tn * t;
  end;

  v := v / XL0[0];

  t2 := t * t;
  t3 := t2 * t; // 千年数的各次方

  v := v + (-0.0728 -2.7702 * t -1.1019 * t2 -0.0996 * t3) / RAD;
  Result := v;
end;

// 计算太阳视黄经，内部调用星历函数日月球面坐标计算中的地球经度计算，传入世纪数、取项数，返回 Date 分点黄经
function SolarApparentLongitude(T, N: Extended): Extended;
begin
  Result := CalcXingLi0(T, N) + EclipticLongitudeNutation(T) + SolarAberration(T) + CN_PI; // 注意这里的章动计算很耗时
end;

// 地球速度计算，T 是世纪数，误差小于万分之 3
function EearthVelocity(T: Extended): Extended;
var
  F: Extended;
begin
  F := 628.307585 * T;
  Result := 628.332 + 21 * Sin(1.527 + F) + 0.44 * Sin(1.48 + F * 2)
    + 0.129 * Sin(5.82 + F) * T + 0.00055 * Sin(4.21 + F) * T * T;
end;

// 根据计算出的太阳视黄经也就是节气对应角度反求日期时间，结果为儒略日数
function GetDateTimeFromSolarApparentLongitude(W: Extended): Extended;
var
  T, V: Extended;
begin
  V := 628.3319653318;
  T := (W - 1.75347 - CN_PI) / V;
  V := EearthVelocity(T); // v 的精度 0.03%
  T := T + ( W - SolarApparentLongitude(T, 10) ) / V;
  V := EearthVelocity(T); // 再算一次 V 有助于提高精度，不算也可以
  T := T + ( W - SolarApparentLongitude(T, -1) ) / V;
  Result := T;
end;

// 基本精确算法之获得某公历年内的第 N 个节气距年初的天数，1-24，对应小寒到冬至。
// 注意小寒有可能为负也就是落到了前一公历年。年数不能为 0，
function GetJieQiDayTimeFromYear(AYear, N: Integer): Extended;
var
  Y: Integer;
  T, JD, JD0: Extended;
begin
  if AYear = 0 then
    raise ECnDateTimeException.Create(SCnErrorYearIsInvalid);

  Y := AYear - 2000; // 把不连续的无 0 公历年变成连续的并且减去 2000
  if Y < -2000 then
    Inc(Y);

  T := GetDateTimeFromSolarApparentLongitude((Y + (N - 6) * 15 / 360 + 1) * 2 * CN_PI);
  // 定气角度
  JD := T * 36525 + J2000 + 8/24 - dt_T(T * 36525);

  Dec(AYear);
  if AYear = 0 then
    Dec(AYear);

  JD0 := GetJulianDate(AYear, 12, 31) - 0.5;
  Result := JD - JD0;
end;

// =========================== 节气精确算法结束 ================================

// 获得某公历年的第 N 个节气的交节月日时分，0-23，对应小寒到冬至
function GetJieQiInAYear(AYear, N: Integer; out AMonth: Integer; out ADay: Integer;
  out AHour: Integer; out AMinitue: Integer; out ASecond: Integer; out ActualYear: Integer): Boolean;
var
  Days: Extended;
  I, Day: Integer;
  Neg: Boolean;
begin
  if not (N in [0..23]) then
    raise ECnDateTimeException.CreateFmt(SCnErrorJieQiIndexIsInvalid, [N]);

  Result := True;
  ActualYear := AYear;

  Days := GetJieQiDayTimeFromYear(AYear, N + 1);
  Neg := Days < 0; // 小于 0 表示在前一年，可能是小寒

  for I := 1 to 12 do
  begin
    Day := GetMonthDays(AYear, I);
    if Days > Day then
      Days := Days - Day
    else
    begin
      AMonth := I;
      Break;
    end;
  end;
  ADay := Floor(Days);

  Days := Days - ADay;
  AHour := Floor(Days * 24);

  Days := Days * 24 - AHour;
  AMinitue := Floor(Days * 60);

  Days := Days * 60 - AMinitue;
  ASecond := Round(Days * 60);

  // 如果秒恰好等于60，则分要加一，如分恰好等于 60，则小时数要加一，如果小时恰好到了 24，则天数要加一
  if ASecond >= 60 then
  begin
    Dec(ASecond, 60);
    Inc(AMinitue);

    if AMinitue >= 60 then
    begin
      Dec(AMinitue, 60);
      Inc(AHour);

      if AHour >= 24 then
      begin
        Dec(AHour, 24);
        Inc(ADay);
      end;

      // 节气不在月底，因此一般不用考虑天数加一后月份改变的情况
    end;
  end;

  if ADay = 0 then // 如果日期是 0，表示是上个月
  begin
    Dec(AMonth);
    if AMonth >= 1 then
      ADay := GetMonthDays(AYear, AMonth)
    else  // 如果月份是 0，表示是去年
    begin
      Dec(AYear);
      if AYear = 0 then // 怕万一碰上公元 0 年
        Dec(AYear);
      ActualYear := AYear;

      AMonth := 12;
      ADay := GetMonthDays(AYear, AMonth);
    end;
  end
  else if Neg and (ADay < 0) then
  begin
    Dec(AYear);
    if AYear = 0 then // 怕万一碰上公元 0 年
      Dec(AYear);
    ActualYear := AYear;

    AMonth := 12;
    ADay := GetMonthDays(AYear, AMonth) + ADay;
  end;
end;

// 获得公历年月日是本年的什么节气，0-23，对应立春到大寒，无则返回 -1
function GetJieQiFromDay(AYear, AMonth, ADay: Integer): Integer;
var
  Month, Day, Idx, TIdx, TYear, DummyHour, DummyMinute, DummySec, DummyActualYear: Integer;
begin
  Result := -1;

  // 每个月两个节气，先算出日期大致对应节气范围再精确计算，以优化性能
  Idx := (AMonth - 1) * 2;
  if ADay >= 15 then
    Inc(Idx);

  GetJieQiInAYear(AYear, Idx, Month, Day, DummyHour, DummyMinute, DummySec, DummyActualYear);
  if (AMonth = Month) and (ADay = Day) then
  begin
    // 此时 I 表示 0 是小寒
    Result := Idx - 2;
    // 转换成 0 是立春
    if Result < 0 then
      Inc(Result, CN_JIEQI_TOTAL_COUNT);
    Exit;
  end;

  // 如果没找着，Idx 的前一个和后一个也得找找
  TIdx := Idx;
  TYear := AYear;

  Inc(Idx);
  if Idx >= CN_JIEQI_TOTAL_COUNT then
  begin
    Dec(Idx, CN_JIEQI_TOTAL_COUNT);
    Inc(AYear);
  end;

  GetJieQiInAYear(AYear, Idx, Month, Day, DummyHour, DummyMinute, DummySec, DummyActualYear);
  if (AMonth = Month) and (ADay = Day) then
  begin
    // 此时 I 表示 0 是小寒
    Result := Idx - 2;
    // 转换成 0 是立春
    if Result < 0 then
      Inc(Result, CN_JIEQI_TOTAL_COUNT);
    Exit;
  end;

  Idx := TIdx;
  AYear := TYear;
  Dec(Idx);
  if Idx < 0 then
  begin
    Inc(Idx, CN_JIEQI_TOTAL_COUNT);
    Dec(AYear);
    if AYear = 0 then // 没有公元 0 年
      Dec(AYear);
  end;

  GetJieQiInAYear(AYear, Idx, Month, Day, DummyHour, DummyMinute, DummySec, DummyActualYear);
  if (AMonth = Month) and (ADay = Day) then
  begin
    // 此时 I 表示 0 是小寒
    Result := Idx - 2;
    // 转换成 0 是立春
    if Result < 0 then
      Inc(Result, CN_JIEQI_TOTAL_COUNT);
    Exit;
  end;
end;

// 获得公历年月日是本年的什么节气以及交节时刻，0-23，对应立春到大寒，无则返回 -1
function GetJieQiTimeFromDay(AYear, AMonth, ADay: Integer; out AHour: Integer;
  out AMinitue: Integer; out ASecond: Integer): Integer;
var
  Month, Day, Idx, TIdx, TYear, DummyActualYear: Integer;
begin
  Result := -1;

  // 每个月两个节气，先算出日期大致对应节气再精确计算，以优化性能
  Idx := (AMonth - 1) * 2;
  if ADay >= 15 then
    Inc(Idx);

  GetJieQiInAYear(AYear, Idx, Month, Day, AHour, AMinitue, ASecond, DummyActualYear);
  if (AMonth = Month) and (ADay = Day) then
  begin
    // 此时 I 表示 0 是小寒
    Result := Idx - 2;
    // 转换成 0 是立春
    if Result < 0 then
      Inc(Result, CN_JIEQI_TOTAL_COUNT);
    Exit;
  end;

  // 如果没找着，Idx 的前一个和后一个也得找找
  TIdx := Idx;
  TYear := AYear;

  Inc(Idx);
  if Idx >= CN_JIEQI_TOTAL_COUNT then
  begin
    Dec(Idx, CN_JIEQI_TOTAL_COUNT);
    Inc(AYear);
  end;

  GetJieQiInAYear(AYear, Idx, Month, Day, AHour, AMinitue, ASecond, DummyActualYear);
  if (AMonth = Month) and (ADay = Day) then
  begin
    // 此时 I 表示 0 是小寒
    Result := Idx - 2;
    // 转换成 0 是立春
    if Result < 0 then
      Inc(Result, CN_JIEQI_TOTAL_COUNT);
    Exit;
  end;

  Idx := TIdx;
  AYear := TYear;
  Dec(Idx);
  if Idx < 0 then
  begin
    Inc(Idx, CN_JIEQI_TOTAL_COUNT);
    Dec(AYear);
    if AYear = 0 then // 没有公元 0 年
      Dec(AYear);
  end;

  GetJieQiInAYear(AYear, Idx, Month, Day, AHour, AMinitue, ASecond, DummyActualYear);
  if (AMonth = Month) and (ADay = Day) then
  begin
    // 此时 I 表示 0 是小寒
    Result := Idx - 2;
    // 转换成 0 是立春
    if Result < 0 then
      Inc(Result, CN_JIEQI_TOTAL_COUNT);
    Exit;
  end;

  AHour := -1;
  AMinitue := -1;
  ASecond := -1;
end;

// 获得某公历时的天干地支，0-59 对应 甲子到癸亥
function GetGanZhiFromHour(AYear, AMonth, ADay, AHour: Integer): Integer;
var
  Gan, Zhi, DummyZhi: Integer;
begin
  AHour := AHour mod 24;
  ExtractGanZhi(GetGanZhiFromDay(AYear, AMonth, ADay), Gan, DummyZhi);

  // Zhi是时辰数(0-11)也就是支数
  if AHour = 23 then
  begin
    // 次日子时
    Gan := (Gan + 1) mod 10;
    Zhi := 0;
  end
  else
  begin
    Inc(AHour);
    Zhi := AHour div 2;
  end;

  // Gan 此时是本日干数，根据规则换算成本日首时辰干数
  if Gan >= 5 then
    Dec(Gan, 5);
  Gan := 2 * Gan;

  // 计算此时辰干数
  Gan := (Gan + Zhi) mod 10;
  Result := CombineGanZhi(Gan, Zhi);
end;

// 获得某公历日的天干地支，0-59 对应 甲子到癸亥
function GetGanZhiFromDay(AllDays: Integer): Integer;
begin
  Result := (AllDays + 12) mod 60;
  if Result < 0 then
    Inc(Result, 60);
end;

function GetGanZhiFromDay(AYear, AMonth, ADay: Integer): Integer;
begin
  Result := GetGanZhiFromDay(GetAllDays(AYear, AMonth, ADay));
end;

// 获得某公历日的天干地支，0-59 对应 甲子到癸亥，小时参数用于判断 23 小时后是次日}
function GetGanZhiFromDay(AYear, AMonth, ADay, AHour: Integer): Integer;
begin
  AHour := AHour mod 24;
  if AHour >= 23 then
    Result := GetGanZhiFromDay(GetAllDays(AYear, AMonth, ADay) + 1)
  else
    Result := GetGanZhiFromDay(GetAllDays(AYear, AMonth, ADay));
end;

// 获得某公历月的天干地支，0-59 对应 甲子到癸亥
function GetGanZhiFromMonth(AYear, AMonth, ADay: Integer): Integer;
begin
  Result := GetGanZhiFromMonth(AYear, AMonth, ADay, 0);
end;

// 获得某公历月的天干地支，0-59 对应 甲子到癸亥
function GetGanZhiFromMonth(AYear, AMonth, ADay, AHour: Integer): Integer;
var
  Gan, DummyZhi, M: Integer;
begin
  // 需要先根据节气调整月份数以及年份数为标准干支纪年
  AdjustYearMonthToGanZhi(AYear, AMonth, ADay, AHour);

  Result := -1;
  ExtractGanZhi(GetGanZhiFromYear(AYear), Gan, DummyZhi);
  case Gan of // 根据口诀从本年干数计算本年首月（立春之后所在的月，一般是二月）的干数
    0,5: // 甲己 丙佐首，
      Result := 2;
    1,6: // 乙庚 戊为头，
      Result := 4;
    2,7: // 丙辛 寻庚起，
      Result := 6;
    3,8: // 丁壬 壬位流，
      Result := 8;
    4,9: // 戊癸 甲好求
      Result := 0;
  end;

  M := AMonth - 1;       // 计算干支纪元的月份 AMonth 与立春后的首月 1 的月份差
  if M < 0 then
    M := M + 10;
  Inc(Result, M mod 10); // 计算本月干数

  if Result >= 10 then
    Result := Result mod 10;

  // 组合支数，立春之后的所在的本月为寅，1 月寅为 2，因而干支月份加 1 即为地支
  Result := CombineGanZhi(Result, (AMonth + 1) mod 12);
end;

// 获得某公/农历年的干支，0-59 对应 甲子到癸亥
function GetGanZhiFromYear(AYear: Integer): Integer;
begin
  if AYear = 0 then
    raise ECnDateTimeException.Create(SCnErrorYearIsInvalid);

  if AYear > 0 then
    Result := (AYear - 4) mod 60
  else // 需要独立判断公元前的原因是没有公元 0 年
    Result := (AYear - 3) mod 60;

  if Result < 0 then
    Inc(Result, 60);
end;

// 根据公历年月日获得某公历年的天干地支，以立春为年分界，0-59 对应 甲子到癸亥
function GetGanZhiFromYear(AYear, AMonth, ADay: Integer): Integer; overload;
begin
  ValidDate(AYear, AMonth, ADay);

  // 如是立春日前，属于前一年。立春当天算这一年
  if GetDayFromYearBegin(AYear, AMonth, ADay) < Floor(GetJieQiDayTimeFromYear(AYear, CN_JIEQI_LICHUN)) then
  begin
    Dec(AYear);
    if AYear = 0 then // 没有公元 0 年
      Dec(AYear);
  end;
  Result := GetGanZhiFromYear(AYear);
end;

// 根据公历年月日获得某公历年的天干地支，以立春为年分界，精确到小时，0-59 对应 甲子到癸亥
function GetGanZhiFromYear(AYear, AMonth, ADay, AHour: Integer): Integer; overload;
begin
  ValidDate(AYear, AMonth, ADay);
  ValidTime(AHour, 0, 0);

  // 如是立春日前，属于前一年，精确到小时判断。立春当天算这一年
  if GetDayFromYearBegin(AYear, AMonth, ADay, AHour) < Floor(GetJieQiDayTimeFromYear(AYear, CN_JIEQI_LICHUN)) then
  begin
    Dec(AYear);
    if AYear = 0 then // 没有公元 0 年
      Dec(AYear);
  end;

  Result := GetGanZhiFromYear(AYear);
end;

// 获得某公/农历年的天干，0-9 对应 甲到癸
function GetGanFromYear(AYear: Integer): Integer;
begin
  if AYear = 0 then
    raise ECnDateTimeException.Create(SCnErrorYearIsInvalid);

  if AYear > 0 then
    Result := (AYear - 4) mod 10
  else // 需要独立判断公元前的原因是没有公元 0 年
    Result := (AYear - 3) mod 10;

  if Result < 0 then
    Inc(Result, 10);
end;

// 获得某公/农历年的地支，0-11 对应 子到亥
function GetZhiFromYear(AYear: Integer): Integer;
begin
  if AYear = 0 then
    raise ECnDateTimeException.Create(SCnErrorYearIsInvalid);

  if AYear > 0 then
    Result := (AYear - 4) mod 12
  else // 需要独立判断公元前的原因是没有公元 0 年
    Result := (AYear - 3) mod 12;

  if Result < 0 then
    Inc(Result, 12);
end;

// 获得某公/农历年的生肖也就是地支，0-11 对应 鼠到猪
function GetShengXiaoFromYear(AYear: Integer): Integer;
begin
  Result := GetZhiFromYear(AYear);
end;

// 获得某公历月日的星座，0-11 对应 白羊到双鱼}
function GetXingZuoFromMonthDay(AMonth, ADay: Integer): Integer;
const
  SCnXingZuoDays: array[0..12] of Integer =
    (120, 219, 321, 421, 521, 622, 723, 823, 923, 1023, 1123, 1222, 1332);
  // 每个星座的起始月日，尾部一个防止超界的大结束号
var
  I, Days: Integer;
begin
  Result := -1;
  Days := AMonth * 100 + ADay;

  for I := 0 to 11 do
  begin
    // 数组内第一个星座是宝瓶所以得加个偏移
    if Days < SCnXingZuoDays[I] then
    begin
      Result := (I + 9) mod 12;
      Exit;
    end
    else if (Days >= SCnXingZuoDays[I]) and (Days < SCnXingZuoDays[I + 1]) then
    begin
      Result := (I + 10) mod 12;
      Exit;
    end;
  end;
end;

// 获得某公历月日的十二建，0-11 对应 建到闭
function Get12JianFromDay(AYear, AMonth, ADay: Integer): Integer;
var
  I, LiChun, JianStart, Days, AllDays, JieQi: Integer;
  DummyGan, Zhi: Integer;
begin
  Result := -1;

  // 十二建类似于地支日轮转，但在非中气的节气那天会重复前一天的
  // 立春后第一个寅日为建日
  JianStart := -1;
  LiChun := Floor(GetJieQiDayTimeFromYear(AYear, CN_JIEQI_LICHUN)); // 获得立春日
  AllDays := GetAllDays(AYear, 1, 1) - 1;

  for I := LiChun + 1 to LiChun + 13 do
  begin
    ExtractGanZhi(GetGanZhiFromDay(AllDays + I), DummyGan, Zhi);

    // 得到了日支数，判断是否寅
    if Zhi = 2 then
    begin
      JianStart := I;
      Break;
    end;
  end;

  Days := GetDayFromYearBegin(AYear, AMonth, ADay);

  // 找到了立春后的第一个寅日
  if JianStart > 0 then
  begin
    // 等于立春建寅
    if JianStart = Days then
    begin
      Result := 0;
      Exit;
    end
    else
    begin
      Result := Days - JianStart; // 先计算差值，调整后再 mod 12 即可

      if Days < JianStart then // 之前之后区分不同节气的情况
      begin
        for I := 3 downto 1 do
        begin
          if (I mod 2 = 0) then Continue; // 不算中气
          JieQi := Floor(GetJieQiDayTimeFromYear(AYear, I));
          if JieQi > Days then // 此节气落在此日期后，表示之后到建寅有十二建的停滞
            Inc(Result);
        end;
      end
      else
      begin
        for I := 4 to 24 do
        begin
          if (I mod 2 = 0) then Continue; // 不算中气
          JieQi := Floor(GetJieQiDayTimeFromYear(AYear, I));
          if JieQi <= Days then // 此节气落在此日期前，表示有十二建的停滞
            Dec(Result);
        end;
      end;

      Result := Result mod 12;
      if Result < 0 then
        Inc(Result, 12);
    end;
  end;
end;

// 获得某公历日的二十八宿，0-27 对应 角到轸
function Get28XiuFromDay(AYear, AMonth, ADay: Integer): Integer;
begin
  // +22 源于公元 1 年 1 月 0 日是柳
  Result := (GetAllDays(AYear, AMonth, ADay) + 22) mod 28;
  if Result < 0 then
    Inc(Result, 28);
end;

// 获得某公历日的农历二十八宿，0-27 对应角到轸
function GetLunar28XiuFromDay(AYear, AMonth, ADay: Integer): Integer;
var
  LY, LM, LD: Integer;
  LP: Boolean;
begin
  Result := -1;
  if GetLunarFromDay(AYear, AMonth, ADay, LY, LM, LD, LP) then
  begin
    // 转换成农历，根据月、日计算
    if (LM in [1..12]) and (LD in [1..30]) then
      Result := SCnLunar28XiuNumber[LM, LD];
  end;
end;

// 获得某公历日的胎神方位，0-59 返回胎神位置加胎神方位的字符串
function GetTaiShenStringFromDay(AYear, AMonth, ADay: Integer): string; overload;
var
  GanZhi: Integer;
begin
  GanZhi := GetGanZhiFromDay(AYear, AMonth, ADay);
  Result := SCnTaiShen1Array[GanZhi] + SCnTaiShen2Array[GanZhi];
end;

// 获得某公历日的胎神方位，0-59 返回胎神位置与胎神方位两个字符串
function GetTaiShenStringFromDay(AYear, AMonth, ADay: Integer;
  out TaiShen1: string; out TaiShen2: string): Boolean;
var
  GanZhi: Integer;
begin
  GanZhi := GetGanZhiFromDay(AYear, AMonth, ADay);
  TaiShen1 := SCnTaiShen1Array[GanZhi];
  TaiShen2 := SCnTaiShen2Array[GanZhi];
  Result := True;
end;

// 获得小时时刻对应的时辰，0-11 对应子至亥
function GetShiChenFromHour(AHour: Integer): Integer;
begin
  Result := -1;
  if not (AHour in [0..23]) then
    Exit;

  if AHour = 23 then
  begin
    // 次日子时
    Result := 0;
  end
  else
  begin
    Inc(AHour);
    Result := AHour div 2;
  end;
end;

// 根据立春为界，调整公历年的年月日的年份数，供黄历中针对年的干支等概念的计算
function AdjustYearToGanZhi(var AYear: Integer; AMonth: Integer;
  ADay: Integer; AHour: Integer): Boolean;
var
  Days: Extended;
begin
  Result := GetDateIsValid(AYear, AMonth, ADay);
  if not Result then
    Exit;

  Days := GetDayFromYearBegin(AYear, AMonth, ADay, AHour);

  // 调整年的记录。因为年的天干地支计算是以立春为分界的，
  // 如本日是本公历年的立春日前，则属于前一年。立春本身算这一年
  if Days < Floor(GetJieQiDayTimeFromYear(AYear, CN_JIEQI_LICHUN)) then
  begin
    // 年需要调整为前一年
    Dec(AYear);
    if AYear = 0 then // 怕万一碰上公元 0 年
      Dec(AYear);
  end;
end;

// 根据立春与节气为界，调整公历年的年月日的年份数与月份数到标准干支纪年
function AdjustYearMonthToGanZhi(var AYear: Integer; var AMonth: Integer;
  ADay: Integer; AHour: Integer): Boolean;
var
  Days, JieQi: Extended;
begin
  Result := GetDateIsValid(AYear, AMonth, ADay);
  if not Result then
    Exit;

  Days := GetDayFromYearBegin(AYear, AMonth, ADay, AHour);

  JieQi := Floor(GetJieQiDayTimeFromYear(AYear, CN_JIEQI_LICHUN)); // 2 月的立春
  if Days < JieQi then
  begin
    Dec(AYear);    // 立春之前，是去年，但要注意公历年没有公元 0 年
    if AYear = 0 then
      Dec(AYear);

    JieQi := Floor(GetJieQiDayTimeFromYear(AYear, CN_JIEQI_XIAOHAN)); // 1 月的小寒
    if Days < JieQi then
      AMonth := 11       // 小寒前算干支年 11 月
    else
      AMonth := 12;      // 小寒后立春前算干支年 12 月
  end
  else
  begin
    // 计算本年的节气（不是前一年的），看该日落在哪俩节气内
    // 如果本公历月首节气的距年头的日数大于等于此日，则此日属于上上个月，节气本日属于上月

    // 公历月 AMonth 的第一个节气的序号是 2 * AMonth - 1，如二月第一个节气立春是 3
    JieQi := Floor(GetJieQiDayTimeFromYear(AYear, 2 * AMonth - 1));
    if Days < JieQi then
      Dec(AMonth, 2)
    else
      Dec(AMonth);
  end;
end;

// 从数字获得三元名称，0-2
function Get3YuanFromNumber(A3Yuan: Integer): string;
begin
  Result := '';
  if (A3Yuan >= 0) and (A3Yuan < 3) then
    Result := SCn3YuanArray[A3Yuan];
end;

// 从数字获得九星名称，0-8
function Get9XingFromNumber(A9Xing: Integer): string;
begin
  Result := '';
  if (A9Xing >= 0) and (A9Xing < 9) then
    Result := SCn9XingArray[A9Xing];
end;

// 从数字获得六曜名称，0-5
function Get6YaoFromNumber(A6Yao: Integer): string;
begin
  Result := '';
  if (A6Yao >= 0) and (A6Yao < 6) then
    Result := SCn6YaoArray[A6Yao];
end;

// 获取公历年所属的三元，0-2 对应上元中元下元
function Get3YuanFromYear(AYear, AMonth, ADay: Integer): Integer;
begin
  Result := -1;
  if AYear = 0 then
    Exit;

  AYear := GetYearSeperatedByLiChun(AYear, AMonth, ADay);
  if AYear < 0 then  // 处理无公元 0 年的情况
    Inc(AYear);

  // 1864 年是某一个上元之始
  AYear := (AYear - 1864) mod 180;
  if AYear < 0 then
    Inc(AYear, 180);

  if AYear in [0..59] then
    Result := 0
  else if AYear in [60..119] then
    Result := 1
  else
    Result := 2;
end;

// 获取公历年的运九星，0-8 对应一白到九紫
function GetYun9XingFromYear(AYear, AMonth, ADay: Integer): Integer;
begin
  Result := -1;
  if AYear = 0 then
    Exit;

  AYear := GetYearSeperatedByLiChun(AYear, AMonth, ADay);
  if AYear < 0 then  // 处理无公元 0 年的情况
    Inc(AYear);

  // 1864 年是某一个上元也就是九运之始
  AYear := (AYear - 1864) mod 180;
  if AYear < 0 then
    Inc(AYear, 180);

  Result := AYear div 20;
end;

// 获取公历年的年九星，0-8 对应一白到九紫
function Get9XingFromYear(AYear, AMonth, ADay: Integer): Integer;
var
  Yuan: Integer;
begin
  Result := -1;
  ValidDate(AYear, AMonth, ADay);

  Yuan := Get3YuanFromYear(AYear, AMonth, ADay);
  AYear := GetYearSeperatedByLiChun(AYear, AMonth, ADay);

  AYear := (AYear - 1864) mod 60;
  if AYear < 0 then
    Inc(AYear, 60);

  case Yuan of
    0:       // 上元起一白
      begin
        Result := 8 - ((AYear + 8) mod 9);
      end;
    1:       // 中元起四绿
      begin
        Result := 8 - ((AYear + 5) mod 9);
      end;
    2:       // 下元起七赤
      begin
        Result := 8 - ((AYear + 2) mod 9);
      end;
  end;
end;

// 获取公历月的月九星，0-8 对应一白到九紫
function Get9XingFromMonth(AYear, AMonth, ADay: Integer): Integer;
var
  Zhi: Integer;
begin
  Result := -1;
  if AdjustYearMonthToGanZhi(AYear, AMonth, ADay, 0) then
  begin
    // 得到立春分割的年以及节气分割的月后获取年干支，
    // 注意这里走的是标准干支纪年，也即立春后的首月算正月 1，拿这个月份数计算才符合口诀
    Zhi := GetZhiFromYear(AYear);
    case Zhi of
      0, 3, 6, 9:
        begin
          // 子午卯酉八白起
          Result := 8 - (AMonth mod 9);
        end;
      2, 5, 8, 11:
        begin
          // 寅申巳亥二黑求
          Result := 8 - ((AMonth + 6) mod 9);
        end;
      1, 4, 7, 10:
        begin
          // 辰戌丑未五黄中
          Result := 8 - ((AMonth + 3) mod 9);
        end;
    end;
  end;
end;

// 获取公历日的日九星，0-8 对应一白到九紫
function Get9XingFromDay(AYear, AMonth, ADay: Integer): Integer;
const
  JIEQI_SEQ: array[0..5] of Integer = (0, 4, 8, 12, 16, 20);
  // 冬至（上一年，所以是 0，小寒为 1）、雨水、谷雨、夏至、处暑、霜降六个节气
var
  I, PreYear, GanZhi, AllDays, Days: Integer;
  Matched: Boolean;
  JieQis: array[0..5] of Integer;     // 六个节气日期（距离年首天数）
  JiaZiQians: array[0..5] of Integer; // 六个节气前的第一个甲子日的日期（距离年首天数）
  JiaZiHous: array[0..5] of Integer;  // 六个节气后的第一个甲子日的日期（距离年首天数）
begin
  Result := -1;
  if AYear = 0 then
    Exit;

  if AYear = 1 then
    PreYear := -1
  else
    PreYear := AYear - 1;

  for I := Low(JIEQI_SEQ) to High(JIEQI_SEQ) do
  begin
    if JIEQI_SEQ[I] > 0 then
    begin
      JieQis[I] := Floor(GetJieQiDayTimeFromYear(AYear, JIEQI_SEQ[I]));
      AllDays := GetAllDays(AYear, 1, 1) - 1;
    end
    else
    begin
      JieQis[I] := Floor(GetJieQiDayTimeFromYear(PreYear, JIEQI_SEQ[I] + 24));
      AllDays := GetAllDays(PreYear, 1, 1) - 1;
    end;

    GanZhi := GetGanZhiFromDay(Alldays + JieQis[I]);  // 得到这个节气日的干支
    JiaZiHous[I] := JieQis[I] + (60 - GanZhi);        // 得到六个节气后甲子日的距年首天数，第 0 个为距上一年的
    JiaZiQians[I] := JiaZiHous[I] - 60;               // 得到六个节气前甲子日的距年首天数，第 0 个为距上一年的
  end;

  JiaZiHous[0] := JiaZiHous[0] - 365;
  JiaZiQians[0] := JiaZiQians[0] - 365;
  JieQis[0] := JieQis[0] - 365;          // 均换算成距本年年初的天数
  if IsLeapYear(PreYear) then
  begin
    Dec(JiaZiHous[0]);
    Dec(JiaZiQians[0]);
  end;

  // JiaZiHous 内是六个节气后甲子日的距本年年首的天数，第 0 个可能为负值，表示在去年
  // JiaZiQians 内是六个节气前甲子日的距本年年首的天数，第 0、1 个可能为负值，表示在去年
  Days := GetDayFromYearBegin(AYear, AMonth, ADay);
  for I := High(JiaZiHous) downto Low(JiaZiHous) do
  begin
    Matched := False;
    if (Days >= JieQis[I]) and (Days < JiaZiHous[I]) then
    begin
      // 节气后（含）到后一个甲子日（不含）内，从节气前的甲子日排
      Days := Days - JiaZiQians[I];
      Matched := True;
    end
    else if Days >= JiaZiHous[I] then
    begin
      // 大于等于节气后甲子日，从该节气后甲子日排
      Days := Days - JiaZiHous[I];
      Matched := True;
    end;

    if not Matched then
      Continue;

    case I of
      0:
        begin
          // 冬至前后一白，顺排
          Result := Days mod 9;
        end;
      1:
        begin
          // 雨水前后七赤，顺排
          Result := (Days + 6) mod 9;
        end;
      2:
        begin
          // 谷雨前后四碧，顺排
          Result := (Days + 3) mod 9;
        end;
      3:
        begin
          // 夏至前后九紫，倒排
          Result := 8 - (Days mod 9);
        end;
      4:
        begin
          // 处暑前后三碧，倒排
          Result := 8 - ((Days + 6) mod 9);
        end;
      5:
        begin
          // 霜降前后六白，倒排
          Result := 8 - ((Days + 3) mod 9);
        end;
    end;
    Exit;
  end;
end;

// 获取公历时的时九星，0-8 对应一白到九紫
function Get9XingFromHour(AYear, AMonth, ADay, AHour: Integer): Integer;
var
  SCH, Days, DayGanZhi, DayGan, DayZhi, XiaZhi, DongZhi: Integer;
begin
  Result := -1;
  if AYear = 0 then
    Exit;

  SCH := GetShiChenFromHour(AHour);
  Days := GetDayFromYearBegin(AYear, AMonth, ADay);

  DayGanZhi := GetGanZhiFromDay(AYear, AMonth, ADay, AHour);
  ExtractGanZhi(DayGanZhi, DayGan, DayZhi);

  DongZhi := Floor(GetJieQiDayTimeFromYear(AYear, CN_JIEQI_DONGZHI));
  XiaZhi := Floor(GetJieQiDayTimeFromYear(AYear, CN_JIEQI_XIAZHI));

  if (Days >= XiaZhi) and (Days < DongZhi) then
  begin
    // 夏至后且冬至前，倒排
    case DayZhi of
      0, 3, 6, 9:
        begin
          // 子午卯酉日的子时是九紫
          Result := 8 - (SCH mod 9);
        end;
      2, 5, 8, 11:
        begin
          // 寅申巳亥日的子时是三碧
          Result := 8 - ((SCH + 3) mod 9);
        end;
      1, 4, 7, 10:
        begin
          // 辰戌丑未日的子时是六白
          Result := 8 - ((SCH + 6) mod 9);
        end;
    end;
  end
  else
  begin
    // 冬至后或夏至前，顺排
    case DayZhi of
      0, 3, 6, 9:
        begin
          // 子午卯酉日的子时是一白
          Result := SCH mod 9;
        end;
      2, 5, 8, 11:
        begin
          // 寅申巳亥日的子时是七赤
          Result := (SCH + 6) mod 9;
        end;
      1, 4, 7, 10:
        begin
          // 辰戌丑未日的子时是四绿
          Result := (SCH + 3) mod 9;
        end;
    end;
  end;
end;

// 获取公历日的日六曜，0-5 对应先胜到赤口
function Get6YaoFromDay(AYear, AMonth, ADay: Integer): Integer;
var
  LY, LM, LD: Integer;
  Leap: Boolean;
begin
  // 农历月日相加
  if GetLunarFromDay(AYear, AMonth, ADay, LY, LM, LD, Leap) then
    Result := (LM + LD + 4) mod 6 // 农历月日和除以 6，余数为 0 是大安，所以加 4
  else
    raise ECnDateTimeException.CreateFmt(SCnErrorConvertLunarDate, [AYear, AMonth, ADay]);
end;

// 根据吉神方位数字获得吉神方位名称
function GetJiShenFangWeiFromNumber(AFangWei: Integer): string;
begin
  Result := '';
  if (AFangWei >= 0) and (AFangWei < 8) then
    Result := SCnJiShenFangWeiArray[AFangWei];
end;

// 获得公历年月日的财神方位，0-7
function GetCaiShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
var
  Gan, Zhi: Integer;
begin
  Result := -1;
  ExtractGanZhi(GetGanZhiFromDay(AYear, AMonth, ADay), Gan, Zhi);
  // 口诀：甲乙东北是财神，丙丁向在西南寻。戊己正北坐方位，庚辛正东去安身。壬癸原来正南坐，便是财神方位真
  case Gan of
    0,1: Result := 1; // 甲乙在东北
    2,3: Result := 5; // 丙丁在西南
    4,5: Result := 0; // 戊己在正北
    6,7: Result := 2; // 庚辛在正东
    8,9: Result := 4; // 壬癸在正南
  end;
end;

// 获得公历年月日的喜神方位，0-7
function GetXiShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
var
  Gan, Zhi: Integer;
begin
  Result := -1;
  ExtractGanZhi(GetGanZhiFromDay(AYear, AMonth, ADay), Gan, Zhi);
  // 口诀：己在艮乙庚乾，丙辛坤位喜神安；丁壬本在离宫坐，戊癸原来在巽间。
  // 八卦对应方位：乾，西北；坎，正北；艮，东北；震，正东；巽，东南；离，正南；坤，西南；兑，正西

  case Gan of
    0,5: Result := 1; // 甲己在东北
    1,6: Result := 7; // 乙庚在西北
    2,7: Result := 5; // 丙辛在西南
    3,8: Result := 4; // 丁壬在正南
    4,9: Result := 3; // 戊癸在东南
  end;
end;

// 获得公历年月日的福神方位，0-7
function GetFuShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
var
  Gan, Zhi: Integer;
begin
  Result := -1;
  ExtractGanZhi(GetGanZhiFromDay(AYear, AMonth, ADay), Gan, Zhi);
  // 福神居然有两套口诀：一是此处用的甲己正北是福神，丙辛西北乾宫存。乙庚坤位戊癸艮，丁壬巽上妙追寻。
  // 二是：甲乙东南是福神，丙丁正东是堪宜，戊北己南庚辛坤，壬在乾方癸在酉。筛查后弃用。

  case Gan of
    0, 5: Result := 0; // 甲己在正北
    1, 6: Result := 5; // 乙庚在西南
    2, 7: Result := 7; // 丙辛在西北
    3, 8: Result := 3; // 丁壬在东南
    4, 9: Result := 1; // 戊癸在东北
  end;
end;

// 获得公历年月日的贵神方位，0-7，默认为阳贵
function GetGuiShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
begin
  Result := GetYangGuiShenFangWeiFromDay(AYear, AMonth, ADay);
end;

// 获得公历年月日的阳贵神方位，0-7
function GetYangGuiShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
var
  Gan, Zhi: Integer;
begin
  Result := -1;
  ExtractGanZhi(GetGanZhiFromDay(AYear, AMonth, ADay), Gan, Zhi);

  case Gan of
    0, 1:    Result := 5; // 甲乙在西南
    2:       Result := 6; // 丙在正西
    3:       Result := 7; // 丁在西北
    4, 6, 7: Result := 1; // 戊庚辛在东北
    5:       Result := 0; // 己在正北
    8:       Result := 2; // 壬在正东
    9:       Result := 3; // 癸在东南
  end;
end;

// 获得公历年月日的阴贵神方位，0-7
function GetYingShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
var
  Gan, Zhi: Integer;
begin
  Result := -1;
  ExtractGanZhi(GetGanZhiFromDay(AYear, AMonth, ADay), Gan, Zhi);

  case Gan of
    0:       Result := 1; // 甲在东北
    1:       Result := 0; // 乙在正北
    2:       Result := 7; // 丙在西北
    3:       Result := 6; // 丁在正西
    4, 5, 6: Result := 5; // 戊己庚在西南
    7:       Result := 4; // 辛在正南
    8:       Result := 3; // 壬在东南
    9:       Result := 2; // 癸在正东
  end;
end;

// 获得公历年月日在数九日中的第几九的第几日，1~9,1~9 对应一九到九九，False 为不在数九日内
function GetShu9Day(AYear, AMonth, ADay: Integer; out JiuSeq: Integer; out JiuDay: Integer): Boolean;
var
  DongZhi, Days: Integer;
begin
  Result := False;
  JiuSeq := -1;
  JiuDay := -1;

  DongZhi := Floor(GetJieQiDayTimeFromYear(AYear, CN_JIEQI_DONGZHI));
  Days := GetDayFromYearBegin(AYear, AMonth, ADay);

  if (Days >= DongZhi) and (Days - DongZhi < 81) then // 在今年的九九内
  begin
    Result := True;
    JiuSeq := ((Days - DongZhi) div 9) + 1;
    JiuDay := ((Days - DongZhi) mod 9) + 1;
  end
  else
  begin // 检查是否是前一公历年内的九九
    Dec(AYear);
    if AYear = 0 then
      Dec(AYear);

    // 获得上一年的冬至日
    DongZhi := Floor(GetJieQiDayTimeFromYear(AYear, CN_JIEQI_DONGZHI));

    // 获得此日离上一年年首的长度
    Days := Days + 365;
    if GetIsLeapYear(AYear) then
      Inc(Days);

    if (Days >= DongZhi) and (Days - DongZhi < 81) then
    begin
      Result := True;
      JiuSeq := ((Days - DongZhi) div 9) + 1;
      JiuDay := ((Days - DongZhi) mod 9) + 1;
    end;
  end;
end;

// 获得公历年月日在三伏日中的第几伏的第几日，0~2,1~10（或 20）对应初伏到末伏的伏日，False 为不在伏日内
function Get3FuDay(AYear, AMonth, ADay: Integer; out FuSeq: Integer; out FuDay: Integer): Boolean;
var
  Days, XiaZhi, LiQiu: Integer;
  AllDays, I: Integer;
  Gan, DummyZhi: Integer;
  F1, F2, F3: Integer;
begin
  Result := False;
  FuSeq := -1;
  FuDay := -1;

  Days := GetDayFromYearBegin(AYear, AMonth, ADay);
  XiaZhi := Floor(GetJieQiDayTimeFromYear(AYear, CN_JIEQI_XIAZHI)); // 获得夏至日
  LiQiu := Floor(GetJieQiDayTimeFromYear(AYear, CN_JIEQI_LIQIU));   // 获得立秋日
  AllDays := GetAllDays(AYear, 1, 1) - 1;

  for I := XiaZhi + 1 to XiaZhi + 21 do // 保证包括夏至后第一个庚日的后 10 天，夏至当日不算
  begin
    if ExtractGanZhi(GetGanZhiFromDay(AllDays + I), Gan, DummyZhi) then
    begin
      if Gan = 6 then // 夏至后第一个庚日
      begin
        ExtractMonthDay(I, AYear, AMonth, ADay);

        F1 := I + 20; // 初伏日，第三个庚日
        F2 := I + 30; // 中伏日，第四个庚日

        if (Days >= F1) and (Days < F1 + 10) then
        begin
          Result := True;
          FuSeq := 0;
          FuDay := Days - F1 + 1;
        end
        else if Days >= F2 then // 中伏
        begin
          if (Days < F2 + 10) or // 中伏 10 日内或立秋前 20 日内
            ((Days >= F2 + 10) and (Days < F2 + 20) and (F2 + 10 <= LiQiu)) then
          begin
            Result := True;
            FuSeq := 1;
            FuDay := Days - F2 + 1;
          end;
        end;

        if Result then
          Exit;

        Break;
      end;
    end;
  end;

  for I := LiQiu + 1 to LiQiu + 21 do // 保证包括立秋后第一个庚日的后 10 天，立秋当日不算
  begin
    if ExtractGanZhi(GetGanZhiFromDay(AllDays + I), Gan, DummyZhi) then
    begin
      if Gan = 6 then // 立秋后第一个庚日
      begin
        F3 := I; // 末伏

        if (Days >= F3) and (Days < F3 + 10) then
        begin
          ExtractMonthDay(I, AYear, AMonth, ADay);
          Result := True;
          FuSeq := 2;
          FuDay := Days - F3 + 1;
        end
        else
          Result := False;

        Exit; // 不能再循环了，否则会出现把第二个庚日又误当末伏开始的错误
      end;
    end;
  end;
end;

// 获得某公历年中的入梅日期，梅雨季节的开始日，芒种后的第一个丙日
function GetRuMeiDay(AYear: Integer; out AMonth: Integer; out ADay: Integer): Boolean;
var
  I, MangZhong, AllDays: Integer;
  Gan, DummyZhi: Integer;
begin
  Result := False;
  MangZhong := Floor(GetJieQiDayTimeFromYear(AYear, CN_JIEQI_MANGZHONG)); // 获得芒种日
  AllDays := GetAllDays(AYear, 1, 1) - 1;

  for I := MangZhong + 1 to MangZhong + 21 do
  begin
    if ExtractGanZhi(GetGanZhiFromDay(AllDays + I), Gan, DummyZhi) then
    begin
      if Gan = 2 then // 芒种后第一个丙日
      begin
        ExtractMonthDay(I, AYear, AMonth, ADay);
        Result := True;
        Exit;
      end;
    end;
  end;
end;

// 获得某公历年中的出梅日期，梅雨季节的结束日，小暑后的第一个未日
function GetChuMeiDay(AYear: Integer; out AMonth: Integer; out ADay: Integer): Boolean;
var
  I, XiaoShu, AllDays: Integer;
  DummyGan, Zhi: Integer;
begin
  Result := False;
  XiaoShu := Floor(GetJieQiDayTimeFromYear(AYear, CN_JIEQI_XIAOSHU)); // 获得小暑日
  AllDays := GetAllDays(AYear, 1, 1) - 1;

  for I := XiaoShu + 1 to XiaoShu + 21 do
  begin
    if ExtractGanZhi(GetGanZhiFromDay(AllDays + I), DummyGan, Zhi) then
    begin
      if Zhi = 7 then // 小暑后第一个未日
      begin
        ExtractMonthDay(I, AYear, AMonth, ADay);
        Result := True;
        Exit;
      end;
    end;
  end;
end;

// 根据公历年月日，返回该日所属的以立春分割的年份，也就是说立春日后是今年，否则为去年
function GetYearSeperatedByLiChun(AYear, AMonth, ADay: Integer): Integer;
var
  Days: Extended;
begin
  Result := AYear;
  Days := GetDayFromYearBegin(AYear, AMonth, ADay);

  // 如本日是立春日前，则是属于前一年
  if Days < GetJieQiDayTimeFromYear(AYear, CN_JIEQI_LICHUN) then
  begin
    // 年调整为前一年
    Dec(Result);
    if Result = 0 then // 没有公元 0 年
      Dec(Result);
  end;
end;

// 移植自中国日历类，似乎是获取该公历年之前的闰月数，这里的 AYear 根据 SCnLeapNumber 的定义来说
// 传的是有公元 0 年的连续年份值，也就是说公元前 850 年应该传 -849，计算到下标 -1？
function GetLeapNum(AYear: Integer): Integer;
begin
  // 前面 850 个是公元年份 -850 到 -1 的闰月数，下标从 0 开始，[0..849] 共 850 个是公元前的
  // 如果公元前某年，负值传进来，譬如 -850 年，要对应到下标 0，所以得加 850
  if AYear < 0 then
    Result := SCnLeapNumber[AYear + 848]      // -1 表示的公元前 2 年，要 847
  else
    Result := SCnLeapNumber[AYear - 1 + 849]; // 公元元年 1，下标计算得到 849，0 表示的公元前 1 年要 848
end;

// 移植自中国日历类，这里的 AYear 是公历年份，没有公元 0 年，也即公元前 850 年要传 -850
// 不过内部做了容错，如果传 0 找公元 0 年的闰月，会返回 0 表示没有闰月
function GetLeapMonth(AYear: Integer): Integer;
var
  C: Char;
begin
  C := SCnLeapMonth[AYear + 850]; // 字符串下标以 1 开始。

{$IFDEF UNICODE}
  if CharInSet(C, ['0'..'9']) then
    Result := StrToInt(C)
  else if CharInSet(C , ['a'..'c']) then
    Result := 10 + Ord(C) - Ord('a')
  else
    Result := -1;
{$ELSE}
  if C in ['0'..'9'] then
    Result := StrToInt(C)
  else if C in ['a'..'c'] then
    Result := 10 + Ord(C) - Ord('a')
  else
    Result := -1;
{$ENDIF}
end;

// 获得一大于零的数的小数部分
function GetTail(X: Real): Real;
begin
  if X > 0 then
    Result := X - Trunc(X)
  else
    Result := X + Trunc(X);
end;

// 某角度计算函数，移植自中国日历类
function GetAng(X, T, C1, T0, T2, T3: Real): Real;
begin
  Result := GetTail(C1 * X) * 2 * Pi + T0 - T2 * T * T - T3 * T * T * T;
end;

// 获得某公历年月日的农历日数和该日月相以及日月食类型和时刻，公历年似乎要求 0 连续，
// 也即公元前 1 年要传 0，公元前 850 年要传 -849
function GetLunarMoon(AYear, AMonth, ADay: Integer; out EclipseType: TCnEclipseType;
  out MoonPhase: TCnMoonPhase; out TheTime: Double): Real;
var
  K, K1: Real;
  T, Rpi, Zone, F0, Fc, J0, Aa0, Ab0, Ac0, ShuoTime, WangTime: Real;
  Aa, Ab, Ac, F1, J: Real;
  I, Ms, LunDay, LunDay0, WangDay: Integer;
  S, R, P, Q: Real;
  StdDays: Integer;
begin
  T := (AYear - 1899.5) / 100;
  Ms := Floor((AYear - 1900) * 12.3685);
  Rpi := 180 / Pi;
  Zone := 8;
  F0 := GetAng(Ms, T, 0, 0.75933, 2.172e-4, 1.55e-7)
    + 0.53058868 * Ms - 8.37e-4 * T + Zone / 24 + 0.5;
  Fc := 0.1734 - 3.93e-4 * T;
  J0 := 693595 + 29 * Ms;
  Aa0 := GetAng(Ms, T, 0.08084821133, 359.2242/Rpi, 0.0000333/Rpi, 0.00000347/Rpi);
  Ab0 := GetAng(Ms, T, 7.171366127999999e-2, 306.0253/Rpi, -0.0107306/Rpi, -0.00001236/Rpi);
  Ac0 := GetAng(Ms, T, 0.08519585128, 21.2964/Rpi, 0.0016528/Rpi, 0.00000239/Rpi);

  EclipseType := etNone;
  LunDay := -1;

  ShuoTime := 0;
  WangDay := 0;
  WangTime := 0;

  K1 := -1; K := -1;
  StdDays := GetEquStandardDays(AYear, AMonth, ADay);
  while K <= 13 do
  begin
    Aa := Aa0 + 0.507984293 * K;
    Ab := Ab0 + 6.73377553 * K;
    Ac := Ac0 + 6.818486628 * K;
    F1 := F0 + 1.53058868 * K + Fc * Sin(Aa) - 0.4068 * Sin(Ab)
      + 0.0021 * Sin(2 * Aa) + 0.0161 * Sin(2 * Ab) + 0.0104 * Sin(2 * Ac)
      - 0.0074 * Sin(Aa - Ab) - 0.0051 * Sin(Aa + Ab);

    J := J0 + 28 * K + F1;

    LunDay0 := StdDays - Floor(J);
    if (K = Floor(K)) and (LunDay0 >= 0) and (LunDay0 <= 29) then
    begin
      K1 := K;
      ShuoTime := GetTail(J);
      LunDay := LunDay0 + 1;
    end;

    if (K = K1 + 0.5) then
    begin
      WangTime := GetTail(J);
      WangDay := Floor(J) - (StdDays - LunDay + 1) + 1;
    end;

    if((LunDay = 1) and (K = K1)) or
      ((LunDay = WangDay) and (K = K1 + 0.5)) then
    begin
      if Abs(Sin(Ac))<= 0.36 then
      begin
        S := 5.19595 - 0.0048 * Cos(Aa) + 0.002 * Cos(2 * Aa) - 0.3283 * Cos(Ab)
          - 0.006 * Cos(Aa + Ab) + 0.0041 * Cos(Aa - Ab);
        R := 0.207 * Sin(Aa) + 0.0024 * Sin(2 * Aa) - 0.039 * Sin(Ab)
          + 0.0115 * Sin(2 * Ab) - 0.0073 * Sin(Aa + Ab) - 0.0067 * Sin(Aa - Ab)
          + 0.0117 * Sin(2 * Ac);
        P := Abs(S * Sin(Ac) + R * Cos(Ac));
        Q := 0.0059 + 0.0046 * Cos(Ac) - 0.0182 * Cos(Ab) + 0.0004 * Cos(2 * Ab)
          - 0.0005 * Cos(Aa + Ab);

        if P - Q <= 1.5572 then
        begin
          EclipseType := etSolar; // 日食
          if K <> Floor(K) then
          begin
            if P + Q >= 1.0129 then
              EclipseType := etMoonHalf   // 月偏食
            else
              EclipseType := etMoonFull;  //月全食
          end;
        end;
      end;
    end;

    K := K + 0.5;
  end;

  // 历史上的观测偏差导致的单个农历月首的单日偏差修正（不跨年的情况）
  for I := Low(CN_LUNAR_SINGLE_MONTH_FIX) to High(CN_LUNAR_SINGLE_MONTH_FIX) do
  begin
    if (AYear = CN_LUNAR_SINGLE_MONTH_FIX[I].Year)
      and (((AMonth = CN_LUNAR_SINGLE_MONTH_FIX[I].Month) and (ADay >= CN_LUNAR_SINGLE_MONTH_FIX[I].StartDay))
      or ((AMonth = CN_LUNAR_SINGLE_MONTH_FIX[I].Month + 1) and (ADay <= CN_LUNAR_SINGLE_MONTH_FIX[I].EndDay))) then
    begin
      if CN_LUNAR_SINGLE_MONTH_FIX[I].IncOne then
      begin
        Inc(LunDay);
        if LunDay > 30 then
          LunDay := Lunday - 30;
      end
      else
      begin
        Dec(LunDay);
        if LunDay < 1 then
          LunDay := LunDay + 30;
      end;
    end;
  end;

  // 245.12.6 到 246.1.4 跨年的有偏差导致该月差一天要加上
  if ((AYear = 245) and ((AMonth = 12) and (ADay >= 6)))
    or ((AYear = 246) and ((AMonth = 1) and (ADay <= 4))) then
  begin
    Inc(LunDay);
    if LunDay > 30 then
      LunDay := Lunday - 30;
  end;

  Result := LunDay;

  if LunDay = 1 then // 朔日
  begin
    MoonPhase := mpShuo;
    TheTime := ShuoTime;
  end
  else if LunDay = WangDay then
  begin
    MoonPhase := mpWang;
    TheTime := WangTime;
  end
  else
  begin
    MoonPhase := mpNone;
    TheTime := -1;
  end;
end;

// 获得某农历年的闰月，返回 1~12 对应一月到十二月，返回 0 表示无闰月
function GetLunarLeapMonth(AYear: Integer): Integer;
begin
  Result := GetLeapMonth(AYear);
  if Result < 0 then
    Result := 0;
end;

// 获得某公历年月日的农历月数，如果是闰月则用负值表示。
// 其中公历年份似乎要求传 0 连续，也即公元前 1 年要传 0，公元前 850 年要传 -849
function GetLunarMonth(AYear, AMonth, ADay: Integer): Real;
var
  LunDay: Real;
  aEclipsType: TCnEclipseType;
  aMoonPhase: TCnMoonPhase;
  aTime: Double;
  LeapMons, NMonth, LMY, LMY1: Integer;

  // 小数的求余数
  function GetRemain(X, W: Real): Real;
  begin
    Result := GetTail(X/W) * W;
  end;

begin
  LunDay := GetLunarMoon(AYear, AMonth, ADay, aEclipsType, aMoonPhase, aTime);
  if aTime <> -1 then
    LunDay := LunDay + aTime;
  LunDay := Floor(LunDay - Floor(LunDay / 100) * 100);

  LeapMons := GetLeapNum(AYear);
  NMonth := Round((GetEquStandardDays(AYear, AMonth, ADay)
    - GetEquStandardDays(-849, 1, 21) - LunDay)/ 29.530588) - LeapMons;
  // 这里用 -849 是因为在 GetEquStandardDays 要求的连续年份的限制下，公元前 850 年是 -849

  //历史上的修改月建
  if (AYear < 240) or ((AYear = 240) and (AMonth = 1) and (ADay < 12)) then
    Inc(NMonth);  // 公元 239 年 12 月 13 日农历十二月大，240 年 1 月 12 日增加十二月小但又不叫闰月

  if AYear <= 237 then Dec(NMonth);

  if (AYear < 24) and not ((AYear = 23) and (AMonth = 12) and (ADay = 31)) then  // 23 年 12 月 31 日也不能加 1
    Inc(NMonth);  // 公元 23 年 12 月 2 日十二月小，12 月 31 日增加十二月大但也不叫闰月

  if AYear < 9 then
    Dec(NMonth);
  if AYear <= -255 then
    Inc(NMonth);
  if AYear <= -256 then
    Inc(NMonth, 2);
  if AYear <= -722 then
    Inc(NMonth);

  LMY := GetLeapMonth(AYear);
  LMY1 := GetLeapMonth(AYear - 1);

  Result := Round(GetRemain(NMonth - 3, 12) + 1); // Result 得到阴历月，但所在阴历年有小概率不是 AYear

  if ((Result = LMY1) and (AMonth = 1) and (ADay < LunDay))
    or ((Result = LMY1) and (LMY1 = 12) and (AMonth in [1, 2])) then
    // 上一行条件是小心地补上去的，如果公历去年闰 12 月，今年公历月份 1 或 2，且算出来的农历月是 12，则该农历月是闰月，理论上无需判断 ADay 和 LunDay 的关系
  begin
    Result := -Result;    // 如果 AYear - 1 年末是闰月且该月接到了 AYear 年,则 AYear 年年初也是闰月
  end
  else if Result = LMY then
  begin
    // 如果得到的月份数与当年所闰的月相同，比如 1612 年 1 月 31 号。
    // 上面计算所得的是 11 月，并且 1612 年年底有个闰 11 月，这俩不能混淆
    // 但如果阴历月 1 且闰月 1，大概率是同一年
    if (Result <> 1) and ((AMonth in [1, 2]) and (LMY <> 12)) then
    begin
      // 粗略判断，如果公历月份在年初，且今年闰月不是 12 月，就大概率说明两个月不是一个年的，
      // 所以不是闰月，修正为普通月。但这个修正可能不是太准确

      // 比如 1984 年有闰 10 月，而 1984.1.1 的农历月为 10，
      // 但这是从 1983 年阴历接过来的，所以不是 1984 年的闰 10 月

      Result := Result + 1;
    end
    else if ((AMonth in [1, 2]) or ((AMonth = 3) and (AYear <= 436))) and (Result = 12) then
    begin
      // 还要考虑这种情况，公元 1574 年 1、2 月转农历得到 12 月，且 1574 年闰 12 月，这种情况也不是闰月，要修正为普通月
      // 公元 436 年 3 月 1 日转农历得到 12 月，且 436 年闰 12 月，这种情况也不是闰月，同样要修正为普通月，但 3 月条件限制在公元 436 之前
      Result := 1;  // 12 + 1 - 12 = 1
    end
    else
    begin
      Result := -Result; // 置负表示闰月
    end;
  end
  else
  begin
    // 这里加一的补偿
    if ((Result < LMY) or (AMonth < Result)) and (LMY > 0) then
    begin
      // 如果 AYear 年有闰月但当月未过闰月则前面多扣除了本年的闰月，这里应当补偿
      // 但如果 AYear 跨年了，实际农历年是前一年比如公元 1575 01 01，这里就会漏掉加一，下面再补偿
      Result := Result + 1;
    end
    else if (Result >= 10) and (AMonth in [1, 2]) and (LMY1 > 0 ) and (Result < LMY1) then
    begin
      // 姑且认为公历月 1 或 2 且农历月 10 或以后，必定发生了跨年，因此得拿前一年的来补偿
      Result := Result + 1;
    end;

    Result := Round(GetRemain(Result - 1, 12) + 1);
  end;

{
   公元 239 年 12 月 13 日十二月大后，公元 240 年 1 月 12 日增加十二月小，本来不算闰月，
   但为了以示区分，240 年 1 月 12 日及以后的十二月返回 IsLeapMonth 为 True。
   公元 23 年 12 月 2 日十二月小后，12 月 31 日增加十二月大，也不叫闰月，
   同样为了以示区分，12 月 31 日及以后的十二月返回 IsLeapMonth 为 True。
}

   if Result = 12 then
   begin
     if AYear = 240 then
     begin
       if ((AMonth = 1) and (ADay >= 12)) or ((AMonth = 2) and (ADay <= 9)) then
         Result := -Result;
     end
     else if (AYear = 23) and (AMonth = 12) and (ADay = 31) then
       Result := -Result
     else if (AYear = 24) and (AMonth = 1) and (ADay <= 29) then
       Result := -Result;
   end;
end;

// 获得某公历年月日的农历年月日和是否闰月的信息
function GetLunarFromDay(AYear, AMonth, ADay: Integer;
  out LunarYear, LunarMonth, LunarDay: Integer; out IsLeapMonth: Boolean): Boolean;
var
  aEclipsType: TCnEclipseType;
  aMoonPhase: TCnMoonPhase;
  aTime: Double;
begin
  Result := False;

  // 0 非连续公元年，转成 0 连续公元年，也即公元前 850 年变成 -849
  NonZeroYearToZeroYear(AYear);

  if (AYear >= -849) and (AYear <= 2800) then
  begin
    LunarDay := Floor(GetLunarMoon(AYear, AMonth, ADay, aEclipsType, aMoonPhase, aTime));
    LunarMonth := Floor(GetLunarMonth(AYear, AMonth, ADay));
    IsLeapMonth := LunarMonth < 0;
    if IsLeapMonth then
      LunarMonth := - LunarMonth;
    LunarYear := AYear;

    // 农历在下半年，公历在上半年，则农历应为上一年
    if (LunarMonth > 6) and (AMonth < 6) then
      Dec(LunarYear);

    // 公元后的特殊情况，公历在 12 月，农历在 1 月，则是下一年
    if (LunarMonth = 1) and (AMonth = 12) then
      Inc(LunarYear);

    ZeroYearToNonZeroYear(LunarYear); // 连续的农历年转成非连续的农历年，没有农历 0 年
    Result := True;
  end;
end;

// 获得某公历年月日的农历月日和是否闰月的信息
function GetLunarMonthDayFromDay(AYear, AMonth, ADay: Integer;
  out LunarMonth, LunarDay: Integer; out IsLeapMonth: Boolean): Boolean;
var
  aEclipsType: TCnEclipseType;
  aMoonPhase: TCnMoonPhase;
  aTime: Double;
begin
  Result := False;

  // 0 非连续公元年，转成 0 连续公元年，也即公元前 850 年变成 -849
  NonZeroYearToZeroYear(AYear);

  if (AYear >= -849) and (AYear <= 2800) then
  begin
    LunarDay := Floor(GetLunarMoon(AYear, AMonth, ADay, aEclipsType, aMoonPhase, aTime));
    LunarMonth := Floor(GetLunarMonth(AYear, AMonth, ADay));
    IsLeapMonth := LunarMonth < 0;
    if IsLeapMonth then
      LunarMonth := - LunarMonth;
    Result := True;
  end;
end;

// 获得某农历年月日（加是否闰月）的公历年月日
// 该函数采用反向二分法查找
function GetDayFromLunar(ALunarYear, ALunarMonth, ALunarDay: Integer; IsLeapMonth:
  Boolean; out AYear, AMonth, ADay: Integer): Boolean;
type
  TLunarSearchDirection = (lsdInvalid, lsdUp, lsdDown);
var
  StartYear, StartMonth, StartDay: Integer;
  EndYear, EndMonth, EndDay: Integer;
  StartDays, EndDays, InterDays: Integer;
  TempYear, TempMonth, TempDay: Integer;
  TempLunarYear, TempLunarMonth, TempLunarDay, OldTempLunarMonth: Integer;
  TempIsLeap, Only2: Boolean;
  Lsd: TLunarSearchDirection;
  Count: Integer;
begin
  Result := False;
  if IsLeapMonth and (GetLunarLeapMonth(ALunarYear) <> ALunarMonth)
    and (GetLunarAdditionalLeapMonth(ALunarYear) <> ALunarMonth) then
    Exit; // 该年无此闰月或额外闰月则退出


  // 初始范围为本公历年一月一日到次年十二月三十一日，这样做的前提是历史上正月初一
  // 没有落到公历年年前去。如果有这样的情况，可考虑适当扩大搜索范围，比如从
  // 上一公历年一月一日到次年十二月三十一日，但又可能引发下面对搜索范围判断的错，只能分开处理
  // 确保搜索范围最多只有两年

  if (ALunarYear < 20) and (ALunarMonth in [1, 2]) then
  begin
    // 公元几十年的范围内，正月初一可能落到公历年前，因此搜索年份改成前一年初到今年年底
    StartYear := ALunarYear - 1;
    if StartYear = 0 then
      Dec(StartYear);

    EndYear := ALunarYear;
  end
  else
  begin
    StartYear := ALunarYear;

    EndYear := ALunarYear + 1;
    if EndYear = 0 then // 没有公元 0 年同样没有农历 0 年
      EndYear := 1;
  end;

  StartMonth := 1;
  StartDay := 1;

  EndMonth := 12;
  EndDay := 31;

  StartDays := Trunc(GetJulianDate(StartYear, StartMonth, StartDay));
  EndDays := Trunc(GetJulianDate(EndYear, EndMonth, EndDay));

  Only2 := False;
  Lsd := lsdInvalid;
  TempYear := StartYear;
  TempLunarYear := StartYear;
  OldTempLunarMonth := 0;

  Count := 0;
  while StartDays < EndDays do
  begin
    Inc(Count);
    if Count > 100 then // 避免陷入死循环
      Exit;

    InterDays := (StartDays + EndDays) div 2;
    if Only2 then
      Inc(InterDays);

    if EndDays - StartDays = 1 then
      Only2 := True;

    GetDayFromJulianDate(InterDays, TempYear, TempMonth, TempDay);
    GetLunarMonthDayFromDay(TempYear, TempMonth, TempDay, TempLunarMonth,
      TempLunarDay, TempIsLeap);
    // 此转换不能直接获取年份，故用下面的判断来获取年份

    if (Lsd = lsdInvalid) and (Count = 1) then
    begin
      // 第一次进来时，可能也要调整农历转换年份，如果公历是年底的月，但农历转出来是年初的月，说明属于第二年
      if (TempMonth > 10) and (TempLunarMonth <= 2) then
      begin
        Inc(TempLunarYear);
        if TempLunarYear = 0 then
          TempLunarYear := 1;
      end
      else if (TempLunarYear = StartYear) and (TempMonth = TempLunarMonth) and (TempMonth = 1) then
      begin
        // 第二种情况，第一次的中间点必然是第一年年底前后，如果公历农历月份都为 1 则表示是属于第二年的
        Inc(TempLunarYear);
        if TempLunarYear = 0 then
          TempLunarYear := 1;
      end;
    end;

    case Lsd of
      lsdUp:
        begin
          // 往未来搜索时如果农历月由大变小了，说明跨了年，年份得加一
          if TempLunarMonth < OldTempLunarMonth then
          begin
            Inc(TempLunarYear);
            if TempLunarYear = 0 then
              TempLunarYear := 1;
          end;
        end;
      lsdDown:
        begin
          // 往过去搜索时如果农历月由小变大了，说明跨了年，年份得减一
          if TempLunarMonth > OldTempLunarMonth then
          begin
            Dec(TempLunarYear);
            if TempLunarYear = 0 then
              TempLunarYear := -1;
          end;
        end;
    end;

    case Compare2LunarDay(TempLunarYear, TempLunarMonth, TempLunarDay, TempIsLeap,
      ALunarYear, ALunarMonth, ALunarDay, IsLeapMonth) of
      -1:
        begin
          StartDays := InterDays;
          Lsd := lsdUp; // 往未来搜索
        end;
      0:
        begin
          AYear := TempYear;
          AMonth := TempMonth;
          ADay := TempDay;
          Result := True;
          Exit;
        end;
      1:
        begin
          EndDays := InterDays;
          Lsd := lsdDown; // 往过去搜索
        end;
    end;
    OldTempLunarMonth := TempLunarMonth;
  end;
end;

end.
