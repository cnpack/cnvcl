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
*           因天文计算精度及历史状况复杂的原因，农历在公元前的准确度无法确保与历史实际情况一致，
*           如唐朝末年的频繁修历导致农历月断掉的史实我们未作处理，使用时应注意。
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

  SCnXingZuoArray: array[0..11] of string = (
    '白羊', '金牛', '双子', '巨蟹', '狮子', '处女',
    '天秤', '天蝎', '射手', '摩羯', '宝瓶', '双鱼'
  );
  {* 星座字符串，Zodiac}

  SCn28XiuArray: array[0..27] of string = (
    '角', '亢', '氐', '房', '心', '尾', '箕',  // 东方青龙七宿，0-6
    '斗', '牛', '女', '虚', '危', '室', '壁',  // 北方玄武七宿，7-13
    '奎', '娄', '胃', '昴', '毕', '觜', '参',  // 西方白虎七宿，14-20
    '井', '鬼', '柳', '星', '张', '翼', '轸'   // 南方朱雀七宿，21-27
  );
  {* 二十八宿字符串}

  SCn28XiuLongArray: array[0..27] of string = (
    '角木蛟', '亢金龙', '氐土貉', '房日兔', '心月狐', '尾火虎', '箕水豹',  // 东方青龙七宿，0-6
    '斗木獬', '牛金牛', '女土蝠', '虚日鼠', '危月燕', '室火猪', '壁水獝',  // 北方玄武七宿，7-13
    '奎木狼', '娄金狗', '胃土雉', '昴日鸡', '毕月乌', '觜火猴', '参水猿',  // 西方白虎七宿，14-20
    '井木犴', '鬼金羊', '柳土獐', '星日马', '张月鹿', '翼火蛇', '轸水蚓'   // 南方朱雀七宿，21-27
  );
  {* 二十八宿完整名称字符串}

  SCnLunarMonthLeapName: string = '闰';
  {* 农历闰月字符串}

  SCnLunarMonthName: string = '月';
  {* 农历月字符串}

  SCnLunarMonthNameArray: array[0..11] of string =
    ('正', '二', '三', '四', '五', '六', '七', '八', '九', '十', '十一', '十二');
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
    ('上元', '中元', '下元');
  {* 三元名称字符串}

  SCn9XingArray: array[0..8] of string =
    ('一白', '二黑', '三碧', '四绿', '五黄', '六白', '七赤', '八白', '九紫');
  {* 九星名称字符串}

  SCn9Xing5XingArray: array[0..8] of string =
    ('水', '土', '木', '木', '土', '金', '金', '土', '火');
  {* 九星所属五行名称字符串}

  SCn9XingStarArray: array[0..8] of string =
    ('贪狼', '巨门', '禄存', '文曲', '廉贞', '武曲', '破军', '左辅', '右弼');
  {* 九星的星宿名称字符串}

  SCn6YaoArray: array[0..5] of string =
    ('先胜', '友引', '先负', '佛灭', '大安', '赤口');
  {* 六曜日的名称字符串}

  SCnTaiShen1Array: array[0..59] of string = (
    '占门碓', '碓磨厕', '厨灶炉', '仓库门', '房床厕',
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
    '占门厕', '碓磨炉', '厨灶门', '仓库栖', '占门床'
  );
  {* 每日胎神位置字符串，与六十干支轮排对应}

  SCnTaiShen2Array: array[0..59] of string = (
    '外东南', '外东南', '外正南', '外正南', '外正南',
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
    '外正东', '外东南', '外东南', '外东南', '外东南'
  );
  {* 每日胎神方位字符串，与六十干支轮排对应}

  SCnNaYinWuXingArray: array[0..29] of string = (
    '海中金', '炉中火', '大林木',
    '路旁土', '剑锋金', '山头火',

    '涧下水', '城墙土', '白蜡金',
    '杨柳木', '泉中水', '屋上土',

    '霹雷火', '松柏木', '长流水',
    '沙中金', '山下火', '平地木',

    '壁上土', '金箔金', '佛灯火',
    '天河水', '大驿土', '钗钏金',

    '桑柘木', '大溪水', '沙中土',
    '天上火', '石榴木', '大海水'
  );
  {* 纳音五行字符串，与相邻一对六十干支对应}

  SCnJiShenFangWeiArray: array[0..7] of string = 
    ('正北', '东北', '正东', '东南', '正南', '西南', '正西', '西北');
  {* 吉神方位字符串，对应八卦的八个方向。
     吉神包括喜神、财神、贵神，贵神还包括阴贵、阳贵，默认指阳贵。}

  SCnGanZhiArray: array[0..59] of string = (
    '甲子', '乙丑', '丙寅', '丁卯', '戊辰', '己巳', '庚午', '辛未', '壬申', '癸酉',   // 0-9
    '甲戌', '乙亥', '丙子', '丁丑', '戊寅', '己卯', '庚辰', '辛巳', '壬午', '癸未',   // 10-19
    '甲申', '乙酉', '丙戌', '丁亥', '戊子', '己丑', '庚寅', '辛卯', '壬辰', '癸巳',   // 20-29
    '甲午', '乙未', '丙申', '丁酉', '戊戌', '己亥', '庚子', '辛丑', '壬寅', '癸卯',   // 30-39
    '甲辰', '乙巳', '丙午', '丁未', '戊申', '己酉', '庚戌', '辛亥', '壬子', '癸丑',   // 40-49
    '甲寅', '乙卯', '丙辰', '丁巳', '戊午', '己未', '庚申', '辛酉', '壬戌', '癸亥'    // 50-59
  );
  {* 六十干支字符串，Sexagenary Cycle}

  SCn12TaiSuiArray: array[0..11] of string =
    ( '太岁', '太阳', '丧门', '太阴', '五鬼', '死符', '岁破', '龙德', '白虎', '福德', '天狗', '病符');
  {* 十二太岁名称字符串，与十二地支对应}

  SCn60TaiSuiArray: array[0..59] of string = (
    '金辨', '陈材', '耿章', '沈兴', '赵达', '郭灿', '王济', '李素', '刘旺', '康志',   // 0-9
    '施广', '任保', '郭嘉', '汪文', '鲁先', '龙仲', '董德', '郑但', '陆明', '魏仁',   // 10-19
    '方杰', '蒋崇', '白敏', '封济', '邹铛', '傅佑', '邬桓', '范宁', '彭泰', '徐单',   // 20-29
    '章词', '杨仙', '管仲', '唐杰', '姜武', '谢太', '卢秘', '杨信', '贺谔', '皮时',   // 30-39
    '李诚', '吴遂', '文哲', '缪丙', '徐浩', '程宝', '倪秘', '叶坚', '丘德', '朱得',   // 40-49
    '张朝', '万清', '辛亚', '杨彦', '黎卿', '傅党', '毛梓', '石政', '洪充', '虞程'    // 50-59
  );
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

procedure StepToPreviousDay(var AYear, AMonth, ADay: Integer;
  AllowZeroYear: Boolean = False);
{* 公历年月日往前步进一天，考虑各种闰年、格里高利历删 10 天等因素。支持公历无 0 年
   （-1 是闰年）及公历有 0 年（0 年闰年）两种模式。默认不允许出现公历 0 年。

   参数：
     var AYear, AMonth, ADay: Integer     - 待步进的公历年、月、日
     AllowZeroYear: Boolean               - 是否允许出现公元 0 年以便于特殊场合计算

   返回值：（无）
}

procedure StepToNextDay(var AYear, AMonth, ADay: Integer;
  AllowZeroYear: Boolean = False);
{* 公历年月日往后步进一天，考虑各种闰年、格里高利历删 10 天等因素。支持公历无 0 年
   （-1 是闰年）及公历有 0 年（0 年闰年）两种模式。默认不允许出现公历 0 年。

   参数：
     var AYear, AMonth, ADay: Integer     - 待步进的公历年、月、日
     AllowZeroYear: Boolean               - 是否允许出现公元 0 年以便于特殊场合计算

   返回值：（无）
}

function IsDayBetweenEqual(AYear, AMonth, ADay: Integer; StartYear, StartMonth, StartDay: Integer;
  EndYear, EndMonth, EndDay: Integer): Boolean;
{* 判断公历年月日是否处于两个公历日期闭区间之间，也就是包含相等的情形。

   参数：
     AYear, AMonth, ADay: Integer                         - 待判断的公历年、月、日
     StartYear, StartMonth, StartDay: Integer             - 闭区间起始公历年、月、日
     EndYear, EndMonth, EndDay: Integer                   - 闭区间结束公历年、月、日

   返回值：Boolean                                        - 返回公历日期是否在区间内
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

function Compare2Day(Year1, Month1, Day1, Year2, Month2, Day2: Integer;
  CheckDay: Boolean = True): Integer;
{* 比较两个公历日期，前者大于、等于、小于后者时分别返回 1、0、-1，大于指更靠近未来。

   参数：
     Year1, Month1, Day1: Integer         - 待判断的第一个公历年、月、日
     Year2, Month2, Day2: Integer         - 待判断的第二个公历年、月、日
     CheckDay: Boolean                    - 判断前是否检查公历日期合法

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
    575, 576, 576, 576, 577, 577, 577, 578, 578, 579, 579, 579, 580, 580,
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
    916, 916, 916, 917, 917, 917, 918, 918, 918, 919, 919, 920, 920, 920,
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
    'a0070030b0' + '0' + '80050010a008005001090050020a0060030c0080040010a0060030c0090' + // 到公元 59 年
    // 这里 70030b008005 中，b008 的第二个 0 是公元 0 年的非法数据，搁这占位用
    '050010a0070030b0080050010a0070030b008005001090070040c0080050020a006003' + // 60 开始
    '0c0080040010a0060030c0090050010a0070030b008005001090070040c0080050020a' + // 130
    '0060030c0080040010a0060030c0090050010b0060030c0090050010a0070030b00800' + // 200
    '5001090070040c0080050020a0060030c0080040010a0070030b0080050010a0070040' + // 270
    'c0080050020a0060030c0080040010a0070030c0090050010a0070030b0080050020a0' + // 340
    '060030c0080040010a0060030c0090050050020a0060030c0090050010b0070030c009' + // 410
    '0050010a0070040c0080050020a0060030c0080050020a0060030c0090050010a00700' + // 480
    '30b0080050020a0060040c0080050020b0070030c00a0050010a0070030b0090050020' + // 550
    'a0070030c0080040020a0060030c0090050010a0070030c0090050030b007005001090' + // 620
    '050020c007004001090060020c007005001090060030b0080040020a0060030b008004' + // 690
    '0010a0060030b0080050010a0050040c0080050010a0060030c0080050010a0070030c' + // 760
    '007005001090070030b0070040020a0060030c0080040020a0070030b0090050010a00' + // 830
    '60040c0080050020a0060040c0080050010b0070030c007005001090070030c0080050' + // 900
    '020a0070030c0090050020a0070030c0090050020a0060040c0090050020a0060040c0' + // 970
    '090050010b0070030c0080050030b007004001090060020c008004002090060020a008' + // 1040
    '004001090050030b0080040020a0060040b0080040c00a0060020b0070050010900600' + // 1110
    '30b0070050020a0060020c008004002090070030c008005002090070040c0080040020' + // 1180
    'a0060040b0090050010a0060030b0080050020a0060040c0080050010b007003001080' + // 1250
    '05001090070030c0080050020a007003001090050030a0070030b0090050020a006004' + // 1320
    '0c0090050030b0070040c0090050010c0070040c0080060020b007004001090060020b' + // 1390
    '007003002090060020a008004001090050030b007004001090050040c0080040c00a00' + // 1460
    '60020c007005001090060030b0070050020a0060020c008004002090060030b0080040' + // 1530
    '02090060030b0080040020a0060040b0080040010b0060040020600500307006004002' + // 1600
    '0700500308006004003070050030700600400307005003080060040030700500409006' + // 1670
    '0040030700500409006005002070050030a00600500307005004002060040020600500' + // 1740
    '30020600400307005004090060040030700500408007005003080050040a0060050030' + // 1810
    '7005004002060050030800500400206005002070050040020600500307006004002070' + // 1890
    '050030800600400307005004080060040a006005003080050040020700500409006004' + // 1960
    '002060050030b006005002070050030800600400307005004080060040030700500408' + // 2030 到 2100
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
  TCnLunarDayOffsetType = (lotDecTwo, lotDecOne, lotIncOne, lotIncTwo);
  {* 古代日期偏移，减两天，减一天，加一天，加两天}

  TCnLunarDateFixRange = packed record
  {* 因古代观测精度限制，针对农历日期（大多是月）的公农历转换修正一天的数据}
    SY: Integer;        // 偏差开始的公历年份
    SM: Integer;        // 偏差开始的公历月份，结束一般是下一个月
    SD: Integer;        // 偏差开始的公历日期
    EY: Integer;        // 偏差结束的公历年份
    EM: Integer;        // 偏差结束的公历月份
    ED: Integer;        // 偏差结束的公历日期
    DayOffset: TCnLunarDayOffsetType;    // 加减天数
  end;
  PCnLunarDateFixRange = ^TCnLunarDateFixRange;

  TCnLunarSmallMonthFix = packed record
  {* 大部分公农历转换的误差修正，碰到月首减一天时都是加 30，少部分加 29，记录这些公历年月}
    Y: Integer;
    M: Integer;
    D: Integer;
  end;
  PCnLunarSmallMonthFix = ^TCnLunarSmallMonthFix;

const
  CN_LUNAR_YEAR_MONTH_SMALL_SUB_FIX: array[0..437] of TCnLunarSmallMonthFix = (
  {* 大部分公农历转换的误差修正，碰到月首减一天时都是加 30，少部分应加 29。
     这些公历年月写在此处供纠错，避免本应农历 29 的变成 30，由于数量较多，内部
     需严格降序以备二分查找。注意为方便起见公历年使用 0，也就是公元前 1 年用 0 表示}
    (Y: 594; M: 12; D: 17),
    (Y: 593; M:  9; D: 30),
    (Y: 593; M:  8; D:  2),
    (Y: 592; M:  9; D: 11),
    (Y: 592; M:  7; D: 14),
    (Y: 566; M:  4; D:  5),
    (Y: 468; M:  9; D:  2),
    (Y: 460; M:  9; D: 30),
    (Y: 291; M:  8; D: 11),
    (Y: 283; M:  9; D:  8),
    (Y: 236; M:  7; D: 20),
    (Y: 236; M:  5; D: 22),
    (Y: 236; M:  3; D: 24),
    (Y: 235; M:  5; D:  4),
    (Y: 235; M:  3; D:  6),
    (Y: 232; M: 11; D: 29),
    (Y: 231; M:  9; D: 13),
    (Y: 230; M: 11; D: 22),
    (Y: 230; M:  7; D: 27),
    (Y: 229; M:  9; D:  5),
    (Y: 229; M:  7; D:  8),
    (Y: 229; M:  5; D: 10),
    (Y: 228; M:  8; D: 17),
    (Y: 228; M:  6; D: 19),
    (Y: 228; M:  4; D: 21),
    (Y: 227; M:  6; D:  1),
    (Y: 227; M:  4; D:  3),
    (Y: 224; M:  1; D:  8),
    (Y: 222; M: 12; D: 20),
    (Y: 222; M: 10; D: 22),
    (Y: 221; M: 10; D:  3),
    (Y: 221; M:  8; D:  5),
    (Y: 221; M:  6; D:  7),
    (Y: 220; M:  9; D: 14),
    (Y: 220; M:  7; D: 17),
    (Y: 220; M:  5; D: 19),
    (Y: 219; M:  8; D: 27),
    (Y: 219; M:  6; D: 29),
    (Y: 219; M:  5; D:  1),
    (Y: 217; M:  6; D: 21),
    (Y: 216; M:  4; D:  4),
    (Y: 215; M:  1; D: 17),
    (Y: 214; M: 11; D: 19),
    (Y: 213; M: 10; D: 31),
    (Y: 213; M:  9; D:  2),
    (Y: 213; M:  7; D:  5),
    (Y: 212; M: 10; D: 12),
    (Y: 212; M:  8; D: 14),
    (Y: 212; M:  6; D: 16),
    (Y: 211; M:  9; D: 24),
    (Y: 211; M:  7; D: 27),
    (Y: 211; M:  5; D: 29),
    (Y: 210; M:  7; D:  8),
    (Y: 208; M:  5; D: 02),
    (Y: 207; M:  2; D: 14),
    (Y: 206; M: 12; D: 17),
    (Y: 205; M: 11; D: 28),
    (Y: 205; M:  9; D: 30),
    (Y: 204; M: 11; D:  9),
    (Y: 204; M:  9; D: 11),
    (Y: 204; M:  7; D: 14),
    (Y: 203; M: 10; D: 22),
    (Y: 203; M:  8; D: 24),
    (Y: 203; M:  6; D: 26),
    (Y: 202; M:  8; D:  5),
    (Y: 202; M:  6; D:  7),
    (Y: 200; M:  7; D: 28),
    (Y: 200; M:  5; D: 30),
    (Y: 199; M:  3; D: 14),
    (Y: 197; M: 12; D: 26),
    (Y: 197; M: 10; D: 28),
    (Y: 196; M: 12; D:  7),
    (Y: 196; M: 10; D:  9),
    (Y: 196; M:  8; D: 11),
    (Y: 195; M: 11; D: 19),
    (Y: 195; M:  9; D: 21),
    (Y: 195; M:  7; D: 24),
    (Y: 194; M:  9; D:  2),
    (Y: 194; M:  7; D:  5),
    (Y: 193; M:  8; D: 14),
    (Y: 193; M:  6; D: 16),
    (Y: 192; M:  8; D: 25),
    (Y: 191; M:  6; D:  9),
    (Y: 191; M:  4; D: 11),
    (Y: 190; M:  3; D: 23),
    (Y: 190; M:  1; D: 23),
    (Y: 189; M:  1; D:  4),
    (Y: 188; M: 11; D:  6),
    (Y: 187; M: 10; D: 19),
    (Y: 187; M:  8; D: 21),
    (Y: 186; M:  9; D: 30),
    (Y: 186; M:  8; D:  2),
    (Y: 185; M:  9; D: 11),
    (Y: 185; M:  7; D: 14),
    (Y: 184; M:  9; D: 22),
    (Y: 183; M:  7; D:  7),
    (Y: 182; M:  4; D: 20),
    (Y: 182; M:  2; D: 20),
    (Y: 181; M:  2; D:  1),
    (Y: 180; M: 12; D:  4),
    (Y: 179; M: 11; D: 16),
    (Y: 178; M: 10; D: 28),
    (Y: 178; M:  8; D: 30),
    (Y: 177; M: 10; D:  9),
    (Y: 177; M:  8; D: 11),
    (Y: 176; M:  7; D: 23),
    (Y: 175; M:  8; D:  4),
    (Y: 174; M:  7; D: 16),
    (Y: 174; M:  5; D: 18),
    (Y: 170; M: 11; D: 25),
    (Y: 169; M: 11; D:  6),
    (Y: 169; M:  9; D:  8),
    (Y: 168; M:  8; D: 20),
    (Y: 167; M: 10; D: 30),
    (Y: 167; M:  9; D:  1),
    (Y: 166; M:  8; D: 13),
    (Y: 166; M:  6; D: 15),
    (Y: 165; M:  5; D: 27),
    (Y: 158; M:  9; D: 10),
    (Y: 158; M:  7; D: 13),
    (Y: 157; M:  8; D: 22),
    (Y: 157; M:  6; D: 24),
    (Y: 150; M: 10; D:  8),
    (Y: 149; M:  9; D: 19),
    (Y: 149; M:  7; D: 22),
    (Y: 148; M:  7; D:  3),
    (Y: 142; M: 11; D:  5),
    (Y: 141; M: 10; D: 17),
    (Y: 141; M:  8; D: 19),
    (Y: 141; M:  6; D: 21),
    (Y: 140; M:  7; D: 31),
    (Y: 134; M: 12; D:  3),
    (Y: 133; M: 11; D: 14),
    (Y: 133; M:  9; D: 16),
    (Y: 132; M:  8; D: 28),
    (Y: 132; M:  6; D: 30),
    (Y: 131; M:  8; D: 10),
    (Y: 125; M: 10; D: 14),
    (Y: 124; M:  9; D: 25),
    (Y: 124; M:  7; D: 28),
    (Y: 123; M:  9; D:  7),
    (Y: 117; M: 11; D: 11),
    (Y: 116; M: 10; D: 23),
    (Y: 115; M: 10; D:  5),
    (Y: 115; M:  8; D:  7),
    (Y:  86; M:  6; D: 29),
    (Y:  85; M:  1; D: 14),
    (Y:  84; M:  9; D: 18),
    (Y:  84; M:  7; D: 21),
    (Y:  84; M:  2; D: 24),
    (Y:  83; M: 12; D: 27),
    (Y:  83; M: 10; D: 29),
    (Y:  83; M:  5; D:  5),
    (Y:  83; M:  3; D:  7),
    (Y:  83; M:  2; D:  5),
    (Y:  82; M: 12; D:  8),
    (Y:  82; M: 10; D: 10),
    (Y:  82; M:  2; D: 16),
    (Y:  81; M: 12; D: 19),
    (Y:  81; M: 11; D: 19),
    (Y:  81; M: 10; D: 21),
    (Y:  81; M:  9; D: 21),
    (Y:  80; M: 11; D: 30),
    (Y:  80; M: 10; D: 31),
    (Y:  80; M: 10; D:  2),
    (Y:  80; M:  9; D:  2),
    (Y:  80; M:  8; D:  4),
    (Y:  80; M:  6; D:  6),
    (Y:  79; M: 11; D: 12),
    (Y:  79; M: 10; D: 13),
    (Y:  79; M:  9; D: 14),
    (Y:  79; M:  8; D: 15),
    (Y:  79; M:  7; D: 17),
    (Y:  79; M:  5; D: 19),
    (Y:  78; M: 10; D: 24),
    (Y:  78; M:  8; D: 26),
    (Y:  78; M:  7; D: 27),
    (Y:  78; M:  6; D: 28),
    (Y:  78; M:  4; D: 30),
    (Y:  77; M: 11; D:  4),
    (Y:  77; M:  8; D:  7),
    (Y:  77; M:  6; D:  9),
    (Y:  77; M:  4; D: 11),
    (Y:  76; M:  8; D: 18),
    (Y:  76; M:  5; D: 21),
    (Y:  76; M:  3; D: 23),
    (Y:  76; M:  1; D: 24),
    (Y:  75; M:  7; D: 31),
    (Y:  75; M:  6; D:  2),
    (Y:  75; M:  4; D:  4),
    (Y:  75; M:  3; D:  5),
    (Y:  75; M:  1; D:  5),
    (Y:  74; M: 11; D:  7),
    (Y:  74; M:  3; D: 16),
    (Y:  74; M:  1; D: 16),
    (Y:  73; M: 12; D: 17),
    (Y:  73; M: 11; D: 18),
    (Y:  73; M: 10; D: 19),
    (Y:  72; M: 12; D: 28),
    (Y:  72; M: 11; D: 28),
    (Y:  72; M: 10; D: 30),
    (Y:  72; M:  9; D: 30),
    (Y:  72; M:  9; D:  1),
    (Y:  71; M: 12; D: 10),
    (Y:  71; M: 11; D: 10),
    (Y:  71; M: 10; D: 12),
    (Y:  71; M:  9; D: 12),
    (Y:  71; M:  8; D: 14),
    (Y:  71; M:  6; D: 16),
    (Y:  70; M: 11; D: 21),
    (Y:  70; M:  9; D: 23),
    (Y:  70; M:  8; D: 24),
    (Y:  70; M:  7; D: 26),
    (Y:  70; M:  5; D: 28),
    (Y:  69; M:  9; D:  4),
    (Y:  69; M:  7; D:  7),
    (Y:  69; M:  5; D:  9),
    (Y:  68; M:  9; D: 15),
    (Y:  68; M:  8; D: 16),
    (Y:  68; M:  6; D: 18),
    (Y:  68; M:  4; D: 20),
    (Y:  67; M:  8; D: 28),
    (Y:  67; M:  6; D: 30),
    (Y:  67; M:  5; D: 31),
    (Y:  67; M:  5; D:  2),
    (Y:  67; M:  4; D:  2),
    (Y:  66; M:  6; D: 11),
    (Y:  66; M:  4; D: 13),
    (Y:  66; M:  2; D: 13),
    (Y:  66; M:  1; D: 14),
    (Y:  65; M: 11; D: 16),
    (Y:  65; M:  3; D: 25),
    (Y:  65; M:  1; D: 25),
    (Y:  64; M: 12; D: 26),
    (Y:  64; M: 11; D: 27),
    (Y:  64; M: 10; D: 28),
    (Y:  64; M:  9; D: 29),
    (Y:  64; M:  1; D:  7),
    (Y:  63; M: 11; D:  9),
    (Y:  63; M: 10; D: 10),
    (Y:  63; M:  9; D: 11),
    (Y:  63; M:  7; D: 14),
    (Y:  62; M: 12; D: 19),
    (Y:  62; M: 10; D: 21),
    (Y:  62; M:  9; D: 21),
    (Y:  62; M:  8; D: 23),
    (Y:  62; M:  6; D: 25),
    (Y:  61; M: 10; D:  2),
    (Y:  61; M:  9; D:  2),
    (Y:  61; M:  8; D:  4),
    (Y:  61; M:  6; D:  6),
    (Y:  60; M: 10; D: 13),
    (Y:  60; M:  9; D: 13),
    (Y:  60; M:  7; D: 16),
    (Y:  60; M:  5; D: 18),
    (Y:  59; M:  9; D: 25),
    (Y:  59; M:  7; D: 28),
    (Y:  59; M:  6; D: 28),
    (Y:  59; M:  4; D: 30),
    (Y:  58; M:  7; D:  9),
    (Y:  58; M:  5; D: 11),
    (Y:  58; M:  4; D: 11),
    (Y:  58; M:  3; D: 13),
    (Y:  57; M:  4; D: 22),
    (Y:  57; M:  2; D: 22),
    (Y:  56; M: 12; D: 25),
    (Y:  56; M:  2; D:  4),
    (Y:  55; M: 12; D:  7),
    (Y:  55; M: 11; D:  7),
    (Y:  55; M: 10; D:  9),
    (Y:  55; M:  8; D: 11),
    (Y:  55; M:  1; D: 16),
    (Y:  54; M: 11; D: 18),
    (Y:  54; M: 10; D: 19),
    (Y:  54; M:  9; D: 20),
    (Y:  54; M:  7; D: 23),
    (Y:  53; M: 10; D: 30),
    (Y:  53; M:  9; D: 30),
    (Y:  53; M:  9; D:  1),
    (Y:  53; M:  7; D:  4),
    (Y:  52; M: 10; D: 11),
    (Y:  52; M:  8; D: 13),
    (Y:  52; M:  6; D: 15),
    (Y:  51; M: 10; D: 23),
    (Y:  51; M:  8; D: 25),
    (Y:  51; M:  7; D: 26),
    (Y:  51; M:  5; D: 28),
    (Y:  50; M:  8; D:  6),
    (Y:  50; M:  7; D:  7),
    (Y:  50; M:  6; D:  8),
    (Y:  50; M:  5; D:  9),
    (Y:  49; M:  7; D: 18),
    (Y:  49; M:  5; D: 20),
    (Y:  49; M:  3; D: 22),
    (Y:  49; M:  1; D: 22),
    (Y:  48; M:  3; D:  3),
    (Y:  48; M:  1; D:  4),
    (Y:  47; M: 11; D:  6),
    (Y:  47; M:  9; D:  8),
    (Y:  47; M:  2; D: 13),
    (Y:  46; M: 12; D: 16),
    (Y:  46; M: 10; D: 18),
    (Y:  46; M:  8; D: 20),
    (Y:  45; M: 11; D: 27),
    (Y:  45; M:  9; D: 29),
    (Y:  45; M:  8; D:  1),
    (Y:  44; M: 11; D:  8),
    (Y:  44; M:  9; D: 10),
    (Y:  44; M:  7; D: 13),
    (Y:  43; M: 11; D: 20),
    (Y:  43; M:  8; D: 23),
    (Y:  43; M:  6; D: 25),
    (Y:  42; M:  9; D:  3),
    (Y:  42; M:  8; D:  4),
    (Y:  42; M:  7; D:  6),
    (Y:  42; M:  6; D:  6),
    (Y:  41; M:  8; D: 15),
    (Y:  41; M:  6; D: 17),
    (Y:  41; M:  4; D: 19),
    (Y:  41; M:  2; D: 19),
    (Y:  40; M:  5; D: 29),
    (Y:  40; M:  3; D: 31),
    (Y:  40; M:  2; D:  1),
    (Y:  39; M: 12; D:  4),
    (Y:  39; M:  3; D: 13),
    (Y:  39; M:  1; D: 13),
    (Y:  38; M: 11; D: 15),
    (Y:  38; M:  9; D: 17),
    (Y:  37; M: 12; D: 25),
    (Y:  37; M: 10; D: 27),
    (Y:  37; M:  8; D: 29),
    (Y:  36; M: 12; D:  6),
    (Y:  36; M: 10; D:  8),
    (Y:  36; M:  8; D: 10),
    (Y:  35; M: 12; D: 18),
    (Y:  35; M:  9; D: 20),
    (Y:  35; M:  7; D: 23),
    (Y:  34; M: 10; D:  1),
    (Y:  34; M:  9; D:  1),
    (Y:  34; M:  8; D:  3),
    (Y:  34; M:  7; D:  4),
    (Y:  33; M:  9; D: 12),
    (Y:  33; M:  7; D: 15),
    (Y:  33; M:  6; D: 15),
    (Y:  33; M:  5; D: 17),
    (Y:  32; M:  6; D: 26),
    (Y:  32; M:  4; D: 28),
    (Y:  32; M:  2; D: 29),
    (Y:  32; M:  1; D:  1),
    (Y:  31; M:  4; D: 10),
    (Y:  31; M:  2; D: 10),
    (Y:  30; M: 12; D: 13),
    (Y:  30; M: 10; D: 15),
    (Y:  30; M:  1; D: 22),
    (Y:  29; M: 11; D: 24),
    (Y:  29; M:  9; D: 26),
    (Y:  29; M:  1; D:  3),
    (Y:  28; M: 11; D:  5),
    (Y:  28; M:  9; D:  7),
    (Y:  28; M:  1; D: 15),
    (Y:  27; M: 10; D: 18),
    (Y:  27; M:  8; D: 20),
    (Y:  26; M: 10; D: 29),
    (Y:  26; M:  9; D: 29),
    (Y:  26; M:  8; D:  1),
    (Y:  25; M: 10; D: 10),
    (Y:  25; M:  9; D: 10),
    (Y:  25; M:  8; D: 12),
    (Y:  25; M:  7; D: 13),
    (Y:  25; M:  6; D: 14),
    (Y:  24; M:  9; D: 21),
    (Y:  24; M:  7; D: 24),
    (Y:  24; M:  5; D: 26),
    (Y:  24; M:  3; D: 28),
    (Y:  24; M:  1; D: 28),
    (Y:  23; M: 12; D: 30),
    (Y:  23; M:  7; D:  6),
    (Y:  23; M:  5; D:  8),
    (Y:  23; M:  3; D: 10),
    (Y:  23; M:  1; D: 10),
    (Y:  22; M:  4; D: 19),
    (Y:  22; M:  2; D: 19),
    (Y:  21; M: 12; D: 22),
    (Y:  21; M: 10; D: 24),
    (Y:  21; M:  1; D: 31),
    (Y:  20; M: 12; D:  3),
    (Y:  20; M: 10; D:  5),
    (Y:  19; M: 11; D: 15),
    (Y:  19; M:  9; D: 17),
    (Y:  18; M: 11; D: 26),
    (Y:  18; M: 10; D: 27),
    (Y:  18; M:  8; D: 29),
    (Y:  17; M: 11; D:  7),
    (Y:  17; M: 10; D:  8),
    (Y:  17; M:  9; D:  9),
    (Y:  17; M:  8; D: 10),
    (Y:  16; M: 10; D: 19),
    (Y:  16; M:  8; D: 21),
    (Y:  16; M:  7; D: 22),
    (Y:  16; M:  6; D: 23),
    (Y:  16; M:  4; D: 25),
    (Y:  15; M:  8; D:  3),
    (Y:  15; M:  6; D:  5),
    (Y:  15; M:  4; D:  7),
    (Y:  14; M:  5; D: 17),
    (Y:  14; M:  3; D: 19),
    (Y:  14; M:  1; D: 19),
    (Y:  13; M:  2; D: 28),
    (Y:  12; M: 12; D: 31),
    (Y:  12; M: 11; D:  2),
    (Y:  11; M: 12; D: 13),
    (Y:  11; M: 10; D: 15),
    (Y:  10; M: 11; D: 24),
    (Y:  10; M:  9; D: 26),
    (Y:   9; M: 12; D:  5),
    (Y:   9; M: 11; D:  5),
    (Y:   9; M: 10; D:  7),
    (Y:   9; M:  9; D:  7),
    (Y:   8; M: 11; D: 16),
    (Y:   8; M:  9; D: 18),
    (Y:   8; M:  8; D: 19),
    (Y:   8; M:  7; D: 21),
    (Y:   8; M:  5; D: 23),
    (Y:   7; M:  8; D: 31),
    (Y:   7; M:  7; D:  3),
    (Y:   7; M:  5; D:  5),
    (Y:   6; M:  8; D: 12),
    (Y:   6; M:  6; D: 14),
    (Y:   6; M:  4; D: 16),
    (Y:   5; M:  3; D: 28),
    (Y:   4; M:  1; D: 10),
    (Y:   3; M:  3; D: 21),
    (Y:   2; M: 10; D: 24),
    (Y:   2; M:  1; D:  2),
    (Y:   1; M: 11; D:  4),
    (Y:   1; M: 10; D:  5),
    (Y:   0; M: 12; D: 14),
    (Y:   0; M:  8; D: 18)
  );

  CN_LUNAR_YEAR_MONTH_SMALL_ADD_FIX: array[0..4] of TCnLunarSmallMonthFix = (
  {* 大部分公农历转换的误差修正，碰到月尾加一天时都是判断是否大于 30，少部分判断 29。
     这些公历年月写在此处供纠错，避免本应下月初一的变成本月 30}
    (Y: 566; M: 4; D: 5),
    (Y: 558; M: 5; D: 3),
    (Y: 389; M: 3; D: 13),
    (Y: 381; M: 4; D: 10),
    (Y: 232; M: 11; D: 29)
  );

  CN_LUNAR_DATE_FIX: array[0..3821] of TCnLunarDateFixRange = (
  {* 历史上因观测偏差导致的农历日期范围的公历单日偏差修正，内部需严格升序以备二分查找。
     注意为方便起见公历年使用 0，也就是公元前 1 年用 0 表示}
    (SY:    0; SM:  6; SD: 20; EY:    0; EM:  9; ED: 15; DayOffset: lotDecOne),
    (SY:    0; SM:  9; SD: 16; EY:    0; EM: 10; ED: 16; DayOffset: lotDecTwo),
    (SY:    0; SM: 10; SD: 17; EY:    1; EM:  2; ED: 11; DayOffset: lotDecOne),
    (SY:    1; SM:  7; SD:  9; EY:    1; EM:  8; ED:  7; DayOffset: lotDecOne),
    (SY:    1; SM:  9; SD:  6; EY:    1; EM: 10; ED:  5; DayOffset: lotDecOne),
    (SY:    1; SM: 10; SD:  6; EY:    1; EM: 11; ED:  3; DayOffset: lotDecTwo),
    (SY:    1; SM: 11; SD:  4; EY:    2; EM:  3; ED:  2; DayOffset: lotDecOne),
    (SY:    2; SM:  4; SD:  1; EY:    2; EM:  4; ED: 30; DayOffset: lotDecOne),
    (SY:    2; SM:  7; SD: 28; EY:    2; EM:  8; ED: 26; DayOffset: lotDecOne),
    (SY:    2; SM:  9; SD: 25; EY:    2; EM: 12; ED: 22; DayOffset: lotDecOne),
    (SY:    3; SM:  1; SD: 21; EY:    3; EM:  5; ED: 19; DayOffset: lotDecOne),
    (SY:    3; SM:  6; SD: 18; EY:    3; EM:  7; ED: 17; DayOffset: lotDecOne),
    (SY:    3; SM:  8; SD: 16; EY:    3; EM:  9; ED: 14; DayOffset: lotDecOne),
    (SY:    3; SM: 10; SD: 14; EY:    3; EM: 11; ED: 12; DayOffset: lotDecOne),
    (SY:    3; SM: 12; SD: 12; EY:    4; EM:  3; ED:  9; DayOffset: lotDecOne),
    (SY:    4; SM:  4; SD:  8; EY:    4; EM:  5; ED:  7; DayOffset: lotDecOne),
    (SY:    4; SM:  6; SD:  6; EY:    4; EM:  8; ED:  4; DayOffset: lotDecOne),
    (SY:    4; SM:  9; SD:  3; EY:    4; EM: 10; ED:  2; DayOffset: lotDecOne),
    (SY:    4; SM: 11; SD:  1; EY:    4; EM: 11; ED: 30; DayOffset: lotDecOne),
    (SY:    4; SM: 12; SD: 30; EY:    5; EM:  1; ED: 28; DayOffset: lotDecOne),
    (SY:    5; SM:  2; SD: 27; EY:    5; EM:  5; ED: 26; DayOffset: lotDecOne),
    (SY:    5; SM:  6; SD: 25; EY:    5; EM:  7; ED: 24; DayOffset: lotDecOne),
    (SY:    5; SM:  8; SD: 23; EY:    5; EM: 10; ED: 21; DayOffset: lotDecOne),
    (SY:    5; SM: 11; SD: 20; EY:    5; EM: 12; ED: 19; DayOffset: lotDecOne),
    (SY:    6; SM:  1; SD: 18; EY:    6; EM:  2; ED: 16; DayOffset: lotDecOne),
    (SY:    6; SM:  3; SD: 18; EY:    6; EM: 10; ED: 10; DayOffset: lotDecOne),
    (SY:    7; SM:  2; SD:  6; EY:    7; EM:  3; ED:  7; DayOffset: lotDecOne),
    (SY:    7; SM:  4; SD:  6; EY:    7; EM: 10; ED: 29; DayOffset: lotDecOne),
    (SY:    7; SM: 11; SD: 28; EY:    7; EM: 12; ED: 27; DayOffset: lotDecOne),
    (SY:    8; SM:  4; SD: 24; EY:    8; EM:  8; ED: 19; DayOffset: lotDecOne),
    (SY:    8; SM:  8; SD: 20; EY:    8; EM:  9; ED: 17; DayOffset: lotDecTwo),
    (SY:    8; SM:  9; SD: 18; EY:    9; EM:  1; ED: 14; DayOffset: lotDecOne),
    (SY:    9; SM:  6; SD: 11; EY:    9; EM:  7; ED: 10; DayOffset: lotDecOne),
    (SY:    9; SM:  8; SD:  9; EY:    9; EM:  9; ED:  7; DayOffset: lotDecOne),
    (SY:    9; SM:  9; SD:  8; EY:    9; EM: 10; ED:  6; DayOffset: lotDecTwo),
    (SY:    9; SM: 10; SD:  7; EY:    9; EM: 11; ED:  5; DayOffset: lotDecOne),
    (SY:    9; SM: 11; SD:  6; EY:    9; EM: 12; ED:  4; DayOffset: lotDecTwo),
    (SY:    9; SM: 12; SD:  5; EY:   10; EM:  2; ED:  2; DayOffset: lotDecOne),
    (SY:   10; SM:  6; SD: 30; EY:   10; EM:  7; ED: 29; DayOffset: lotDecOne),
    (SY:   10; SM:  8; SD: 28; EY:   11; EM:  2; ED: 21; DayOffset: lotDecOne),
    (SY:   11; SM:  3; SD: 23; EY:   11; EM:  4; ED: 21; DayOffset: lotDecOne),
    (SY:   11; SM:  5; SD: 21; EY:   11; EM:  6; ED: 19; DayOffset: lotDecOne),
    (SY:   11; SM:  7; SD: 19; EY:   11; EM:  8; ED: 17; DayOffset: lotDecOne),
    (SY:   11; SM:  9; SD: 16; EY:   12; EM:  2; ED: 10; DayOffset: lotDecOne),
    (SY:   12; SM:  3; SD: 11; EY:   12; EM:  5; ED:  9; DayOffset: lotDecOne),
    (SY:   12; SM:  6; SD:  8; EY:   12; EM:  7; ED:  7; DayOffset: lotDecOne),
    (SY:   12; SM:  8; SD:  6; EY:   12; EM:  9; ED:  4; DayOffset: lotDecOne),
    (SY:   12; SM: 10; SD:  4; EY:   13; EM:  4; ED: 28; DayOffset: lotDecOne),
    (SY:   13; SM:  5; SD: 28; EY:   13; EM:  6; ED: 26; DayOffset: lotDecOne),
    (SY:   13; SM:  8; SD: 25; EY:   13; EM:  9; ED: 23; DayOffset: lotDecOne),
    (SY:   13; SM: 10; SD: 23; EY:   13; EM: 11; ED: 21; DayOffset: lotDecOne),
    (SY:   13; SM: 12; SD: 21; EY:   14; EM:  7; ED: 15; DayOffset: lotDecOne),
    (SY:   14; SM:  8; SD: 14; EY:   14; EM:  9; ED: 12; DayOffset: lotDecOne),
    (SY:   14; SM: 11; SD: 11; EY:   14; EM: 12; ED: 10; DayOffset: lotDecOne),
    (SY:   15; SM:  1; SD:  9; EY:   15; EM:  2; ED:  7; DayOffset: lotDecOne),
    (SY:   15; SM:  3; SD:  9; EY:   15; EM: 10; ED:  1; DayOffset: lotDecOne),
    (SY:   15; SM: 10; SD: 31; EY:   15; EM: 11; ED: 29; DayOffset: lotDecOne),
    (SY:   16; SM:  3; SD: 27; EY:   16; EM:  7; ED: 22; DayOffset: lotDecOne),
    (SY:   16; SM:  7; SD: 23; EY:   16; EM:  8; ED: 20; DayOffset: lotDecTwo),
    (SY:   16; SM:  8; SD: 21; EY:   16; EM: 12; ED: 17; DayOffset: lotDecOne),
    (SY:   17; SM:  5; SD: 14; EY:   17; EM:  6; ED: 12; DayOffset: lotDecOne),
    (SY:   17; SM:  7; SD: 12; EY:   17; EM:  8; ED: 10; DayOffset: lotDecOne),
    (SY:   17; SM:  8; SD: 11; EY:   17; EM:  9; ED:  8; DayOffset: lotDecTwo),
    (SY:   17; SM:  9; SD:  9; EY:   17; EM: 10; ED:  8; DayOffset: lotDecOne),
    (SY:   17; SM: 10; SD:  9; EY:   17; EM: 11; ED:  6; DayOffset: lotDecTwo),
    (SY:   17; SM: 11; SD:  7; EY:   18; EM:  1; ED:  5; DayOffset: lotDecOne),
    (SY:   18; SM:  6; SD:  2; EY:   18; EM:  7; ED:  1; DayOffset: lotDecOne),
    (SY:   18; SM:  7; SD: 31; EY:   18; EM: 10; ED: 27; DayOffset: lotDecOne),
    (SY:   18; SM: 10; SD: 28; EY:   18; EM: 11; ED: 25; DayOffset: lotDecTwo),
    (SY:   18; SM: 11; SD: 26; EY:   19; EM:  1; ED: 24; DayOffset: lotDecOne),
    (SY:   19; SM:  2; SD: 23; EY:   19; EM:  3; ED: 24; DayOffset: lotDecOne),
    (SY:   19; SM:  6; SD: 21; EY:   19; EM:  7; ED: 20; DayOffset: lotDecOne),
    (SY:   19; SM:  8; SD: 19; EY:   20; EM:  1; ED: 13; DayOffset: lotDecOne),
    (SY:   20; SM:  2; SD: 12; EY:   20; EM:  4; ED: 11; DayOffset: lotDecOne),
    (SY:   20; SM:  5; SD: 11; EY:   20; EM:  6; ED:  9; DayOffset: lotDecOne),
    (SY:   20; SM:  7; SD:  9; EY:   20; EM:  8; ED:  7; DayOffset: lotDecOne),
    (SY:   20; SM:  9; SD:  6; EY:   21; EM:  3; ED: 31; DayOffset: lotDecOne),
    (SY:   21; SM:  4; SD: 30; EY:   21; EM:  6; ED: 28; DayOffset: lotDecOne),
    (SY:   21; SM:  7; SD: 28; EY:   21; EM:  8; ED: 26; DayOffset: lotDecOne),
    (SY:   21; SM:  9; SD: 25; EY:   22; EM:  6; ED: 17; DayOffset: lotDecOne),
    (SY:   22; SM:  7; SD: 17; EY:   22; EM:  8; ED: 15; DayOffset: lotDecOne),
    (SY:   22; SM: 10; SD: 14; EY:   22; EM: 11; ED: 12; DayOffset: lotDecOne),
    (SY:   22; SM: 12; SD: 12; EY:   23; EM:  9; ED:  3; DayOffset: lotDecOne),
    (SY:   23; SM: 10; SD:  3; EY:   23; EM: 11; ED:  1; DayOffset: lotDecOne),
    (SY:   24; SM:  2; SD: 28; EY:   24; EM: 11; ED: 19; DayOffset: lotDecOne),
    (SY:   25; SM:  4; SD: 16; EY:   25; EM:  7; ED: 13; DayOffset: lotDecOne),
    (SY:   25; SM:  7; SD: 14; EY:   25; EM:  8; ED: 11; DayOffset: lotDecTwo),
    (SY:   25; SM:  8; SD: 12; EY:   25; EM:  9; ED: 10; DayOffset: lotDecOne),
    (SY:   25; SM:  9; SD: 11; EY:   25; EM: 10; ED:  9; DayOffset: lotDecTwo),
    (SY:   25; SM: 10; SD: 10; EY:   25; EM: 12; ED:  8; DayOffset: lotDecOne),
    (SY:   26; SM:  5; SD:  5; EY:   26; EM:  6; ED:  3; DayOffset: lotDecOne),
    (SY:   26; SM:  7; SD:  3; EY:   26; EM:  9; ED: 29; DayOffset: lotDecOne),
    (SY:   26; SM:  9; SD: 30; EY:   26; EM: 10; ED: 28; DayOffset: lotDecTwo),
    (SY:   26; SM: 10; SD: 29; EY:   26; EM: 12; ED: 27; DayOffset: lotDecOne),
    (SY:   27; SM:  1; SD: 26; EY:   27; EM:  2; ED: 24; DayOffset: lotDecOne),
    (SY:   27; SM:  5; SD: 24; EY:   27; EM:  6; ED: 22; DayOffset: lotDecOne),
    (SY:   27; SM:  7; SD: 22; EY:   28; EM:  3; ED: 14; DayOffset: lotDecOne),
    (SY:   28; SM:  4; SD: 13; EY:   28; EM:  5; ED: 12; DayOffset: lotDecOne),
    (SY:   28; SM:  6; SD: 11; EY:   28; EM:  7; ED: 10; DayOffset: lotDecOne),
    (SY:   28; SM:  8; SD:  9; EY:   29; EM:  3; ED:  3; DayOffset: lotDecOne),
    (SY:   29; SM:  5; SD:  2; EY:   29; EM:  5; ED: 31; DayOffset: lotDecOne),
    (SY:   29; SM:  6; SD: 30; EY:   29; EM:  7; ED: 29; DayOffset: lotDecOne),
    (SY:   29; SM:  8; SD: 28; EY:   30; EM:  3; ED: 22; DayOffset: lotDecOne),
    (SY:   30; SM:  4; SD: 21; EY:   30; EM:  5; ED: 20; DayOffset: lotDecOne),
    (SY:   30; SM:  6; SD: 19; EY:   30; EM:  7; ED: 18; DayOffset: lotDecOne),
    (SY:   30; SM:  9; SD: 16; EY:   31; EM:  6; ED:  8; DayOffset: lotDecOne),
    (SY:   31; SM:  7; SD:  8; EY:   31; EM:  8; ED:  6; DayOffset: lotDecOne),
    (SY:   31; SM:  9; SD:  5; EY:   31; EM: 10; ED:  4; DayOffset: lotDecOne),
    (SY:   31; SM: 11; SD:  3; EY:   32; EM:  8; ED: 24; DayOffset: lotDecOne),
    (SY:   32; SM:  9; SD: 23; EY:   32; EM: 10; ED: 22; DayOffset: lotDecOne),
    (SY:   33; SM:  3; SD: 19; EY:   33; EM:  6; ED: 15; DayOffset: lotDecOne),
    (SY:   33; SM:  6; SD: 16; EY:   33; EM:  7; ED: 14; DayOffset: lotDecTwo),
    (SY:   33; SM:  7; SD: 15; EY:   33; EM: 11; ED: 10; DayOffset: lotDecOne),
    (SY:   33; SM: 12; SD: 10; EY:   34; EM:  1; ED:  8; DayOffset: lotDecOne),
    (SY:   34; SM:  4; SD:  7; EY:   34; EM:  5; ED:  6; DayOffset: lotDecOne),
    (SY:   34; SM:  6; SD:  5; EY:   34; EM:  7; ED:  4; DayOffset: lotDecOne),
    (SY:   34; SM:  7; SD:  5; EY:   34; EM:  8; ED:  2; DayOffset: lotDecTwo),
    (SY:   34; SM:  8; SD:  3; EY:   34; EM:  9; ED:  1; DayOffset: lotDecOne),
    (SY:   34; SM:  9; SD:  2; EY:   34; EM:  9; ED: 30; DayOffset: lotDecTwo),
    (SY:   34; SM: 10; SD:  1; EY:   34; EM: 11; ED: 29; DayOffset: lotDecOne),
    (SY:   34; SM: 12; SD: 29; EY:   35; EM:  1; ED: 27; DayOffset: lotDecOne),
    (SY:   35; SM:  4; SD: 26; EY:   35; EM:  5; ED: 25; DayOffset: lotDecOne),
    (SY:   35; SM:  6; SD: 24; EY:   36; EM:  2; ED: 15; DayOffset: lotDecOne),
    (SY:   36; SM:  3; SD: 16; EY:   36; EM:  4; ED: 14; DayOffset: lotDecOne),
    (SY:   36; SM:  5; SD: 14; EY:   36; EM:  6; ED: 12; DayOffset: lotDecOne),
    (SY:   36; SM:  7; SD: 12; EY:   37; EM:  2; ED:  3; DayOffset: lotDecOne),
    (SY:   37; SM:  4; SD:  4; EY:   37; EM:  5; ED:  3; DayOffset: lotDecOne),
    (SY:   37; SM:  6; SD:  2; EY:   37; EM:  7; ED:  1; DayOffset: lotDecOne),
    (SY:   37; SM:  7; SD: 31; EY:   38; EM:  2; ED: 22; DayOffset: lotDecOne),
    (SY:   38; SM:  3; SD: 24; EY:   38; EM:  4; ED: 22; DayOffset: lotDecOne),
    (SY:   38; SM:  6; SD: 21; EY:   38; EM:  7; ED: 20; DayOffset: lotDecOne),
    (SY:   38; SM:  8; SD: 19; EY:   39; EM:  5; ED: 11; DayOffset: lotDecOne),
    (SY:   39; SM:  6; SD: 10; EY:   39; EM:  7; ED:  9; DayOffset: lotDecOne),
    (SY:   39; SM:  8; SD:  8; EY:   39; EM:  9; ED:  6; DayOffset: lotDecOne),
    (SY:   39; SM: 10; SD:  6; EY:   40; EM:  7; ED: 27; DayOffset: lotDecOne),
    (SY:   40; SM:  8; SD: 26; EY:   40; EM:  9; ED: 24; DayOffset: lotDecOne),
    (SY:   40; SM: 10; SD: 24; EY:   40; EM: 11; ED: 22; DayOffset: lotDecOne),
    (SY:   40; SM: 12; SD: 22; EY:   41; EM: 10; ED: 13; DayOffset: lotDecOne),
    (SY:   41; SM: 11; SD: 12; EY:   41; EM: 12; ED: 11; DayOffset: lotDecOne),
    (SY:   42; SM:  1; SD: 10; EY:   42; EM:  2; ED:  8; DayOffset: lotDecOne),
    (SY:   42; SM:  3; SD: 10; EY:   42; EM:  4; ED:  8; DayOffset: lotDecOne),
    (SY:   42; SM:  5; SD:  8; EY:   42; EM:  6; ED:  6; DayOffset: lotDecOne),
    (SY:   42; SM:  6; SD:  7; EY:   42; EM:  7; ED:  5; DayOffset: lotDecTwo),
    (SY:   42; SM:  7; SD:  6; EY:   42; EM:  8; ED:  4; DayOffset: lotDecOne),
    (SY:   42; SM:  8; SD:  5; EY:   42; EM:  9; ED:  2; DayOffset: lotDecTwo),
    (SY:   42; SM:  9; SD:  3; EY:   42; EM: 11; ED:  1; DayOffset: lotDecOne),
    (SY:   42; SM: 12; SD:  1; EY:   42; EM: 12; ED: 30; DayOffset: lotDecOne),
    (SY:   43; SM:  3; SD: 29; EY:   43; EM:  4; ED: 27; DayOffset: lotDecOne),
    (SY:   43; SM:  5; SD: 27; EY:   44; EM:  1; ED: 18; DayOffset: lotDecOne),
    (SY:   44; SM:  2; SD: 17; EY:   44; EM:  3; ED: 17; DayOffset: lotDecOne),
    (SY:   44; SM:  4; SD: 16; EY:   44; EM:  5; ED: 15; DayOffset: lotDecOne),
    (SY:   44; SM:  6; SD: 14; EY:   45; EM:  2; ED:  5; DayOffset: lotDecOne),
    (SY:   45; SM:  3; SD:  7; EY:   45; EM:  4; ED:  5; DayOffset: lotDecOne),
    (SY:   45; SM:  5; SD:  5; EY:   45; EM:  6; ED:  3; DayOffset: lotDecOne),
    (SY:   45; SM:  7; SD:  3; EY:   46; EM:  1; ED: 25; DayOffset: lotDecOne),
    (SY:   46; SM:  2; SD: 24; EY:   46; EM:  3; ED: 25; DayOffset: lotDecOne),
    (SY:   46; SM:  5; SD: 24; EY:   46; EM:  6; ED: 22; DayOffset: lotDecOne),
    (SY:   46; SM:  7; SD: 22; EY:   47; EM:  4; ED: 13; DayOffset: lotDecOne),
    (SY:   47; SM:  5; SD: 13; EY:   47; EM:  6; ED: 11; DayOffset: lotDecOne),
    (SY:   47; SM:  7; SD: 11; EY:   48; EM:  5; ED:  1; DayOffset: lotDecOne),
    (SY:   48; SM:  5; SD: 31; EY:   48; EM:  6; ED: 29; DayOffset: lotDecOne),
    (SY:   48; SM:  7; SD: 29; EY:   48; EM:  8; ED: 27; DayOffset: lotDecOne),
    (SY:   48; SM:  9; SD: 26; EY:   48; EM: 10; ED: 25; DayOffset: lotDecOne),
    (SY:   48; SM: 11; SD: 24; EY:   49; EM:  9; ED: 15; DayOffset: lotDecOne),
    (SY:   49; SM: 10; SD: 15; EY:   49; EM: 11; ED: 13; DayOffset: lotDecOne),
    (SY:   49; SM: 12; SD: 13; EY:   50; EM:  1; ED: 11; DayOffset: lotDecOne),
    (SY:   50; SM:  2; SD: 10; EY:   50; EM:  3; ED: 11; DayOffset: lotDecOne),
    (SY:   50; SM:  4; SD: 10; EY:   50; EM:  5; ED:  9; DayOffset: lotDecOne),
    (SY:   50; SM:  5; SD: 10; EY:   50; EM:  6; ED:  7; DayOffset: lotDecTwo),
    (SY:   50; SM:  6; SD:  8; EY:   50; EM:  7; ED:  7; DayOffset: lotDecOne),
    (SY:   50; SM:  7; SD:  8; EY:   50; EM:  8; ED:  5; DayOffset: lotDecTwo),
    (SY:   50; SM:  8; SD:  6; EY:   50; EM: 10; ED:  4; DayOffset: lotDecOne),
    (SY:   50; SM: 11; SD:  3; EY:   50; EM: 12; ED:  2; DayOffset: lotDecOne),
    (SY:   51; SM:  1; SD:  1; EY:   51; EM:  1; ED: 30; DayOffset: lotDecOne),
    (SY:   51; SM:  3; SD:  1; EY:   51; EM:  3; ED: 30; DayOffset: lotDecOne),
    (SY:   51; SM:  4; SD: 29; EY:   51; EM:  7; ED: 26; DayOffset: lotDecOne),
    (SY:   51; SM:  7; SD: 27; EY:   51; EM:  8; ED: 24; DayOffset: lotDecTwo),
    (SY:   51; SM:  8; SD: 25; EY:   51; EM: 12; ED: 21; DayOffset: lotDecOne),
    (SY:   52; SM:  1; SD: 20; EY:   52; EM:  2; ED: 18; DayOffset: lotDecOne),
    (SY:   52; SM:  3; SD: 19; EY:   52; EM:  4; ED: 17; DayOffset: lotDecOne),
    (SY:   52; SM:  5; SD: 17; EY:   53; EM:  1; ED:  8; DayOffset: lotDecOne),
    (SY:   53; SM:  2; SD:  7; EY:   53; EM:  3; ED:  8; DayOffset: lotDecOne),
    (SY:   53; SM:  4; SD:  7; EY:   53; EM:  5; ED:  6; DayOffset: lotDecOne),
    (SY:   53; SM:  6; SD:  5; EY:   53; EM:  9; ED: 30; DayOffset: lotDecOne),
    (SY:   53; SM: 10; SD:  1; EY:   53; EM: 10; ED: 29; DayOffset: lotDecTwo),
    (SY:   53; SM: 10; SD: 30; EY:   53; EM: 12; ED: 28; DayOffset: lotDecOne),
    (SY:   54; SM:  1; SD: 27; EY:   54; EM:  3; ED: 27; DayOffset: lotDecOne),
    (SY:   54; SM:  4; SD: 26; EY:   54; EM:  5; ED: 25; DayOffset: lotDecOne),
    (SY:   54; SM:  6; SD: 24; EY:   54; EM: 10; ED: 19; DayOffset: lotDecOne),
    (SY:   54; SM: 10; SD: 20; EY:   54; EM: 11; ED: 17; DayOffset: lotDecTwo),
    (SY:   54; SM: 11; SD: 18; EY:   55; EM:  3; ED: 16; DayOffset: lotDecOne),
    (SY:   55; SM:  7; SD: 13; EY:   55; EM: 11; ED:  7; DayOffset: lotDecOne),
    (SY:   55; SM: 11; SD:  8; EY:   55; EM: 12; ED:  6; DayOffset: lotDecTwo),
    (SY:   55; SM: 12; SD:  7; EY:   56; EM:  4; ED:  3; DayOffset: lotDecOne),
    (SY:   56; SM:  5; SD:  3; EY:   56; EM:  6; ED:  1; DayOffset: lotDecOne),
    (SY:   56; SM:  7; SD:  1; EY:   56; EM:  7; ED: 30; DayOffset: lotDecOne),
    (SY:   56; SM:  8; SD: 29; EY:   56; EM:  9; ED: 27; DayOffset: lotDecOne),
    (SY:   56; SM: 10; SD: 27; EY:   57; EM:  6; ED: 20; DayOffset: lotDecOne),
    (SY:   57; SM:  7; SD: 20; EY:   57; EM:  8; ED: 18; DayOffset: lotDecOne),
    (SY:   57; SM:  9; SD: 17; EY:   57; EM: 10; ED: 16; DayOffset: lotDecOne),
    (SY:   57; SM: 11; SD: 15; EY:   57; EM: 12; ED: 14; DayOffset: lotDecOne),
    (SY:   58; SM:  1; SD: 13; EY:   58; EM:  4; ED: 11; DayOffset: lotDecOne),
    (SY:   58; SM:  4; SD: 12; EY:   58; EM:  5; ED: 10; DayOffset: lotDecTwo),
    (SY:   58; SM:  5; SD: 11; EY:   58; EM:  9; ED:  6; DayOffset: lotDecOne),
    (SY:   58; SM: 10; SD:  6; EY:   58; EM: 11; ED:  4; DayOffset: lotDecOne),
    (SY:   58; SM: 12; SD:  4; EY:   59; EM:  1; ED:  2; DayOffset: lotDecOne),
    (SY:   59; SM:  2; SD:  1; EY:   59; EM:  3; ED:  2; DayOffset: lotDecOne),
    (SY:   59; SM:  4; SD:  1; EY:   59; EM:  6; ED: 28; DayOffset: lotDecOne),
    (SY:   59; SM:  6; SD: 29; EY:   59; EM:  7; ED: 27; DayOffset: lotDecTwo),
    (SY:   59; SM:  7; SD: 28; EY:   59; EM: 11; ED: 23; DayOffset: lotDecOne),
    (SY:   59; SM: 12; SD: 23; EY:   60; EM:  1; ED: 21; DayOffset: lotDecOne),
    (SY:   60; SM:  2; SD: 20; EY:   60; EM:  3; ED: 20; DayOffset: lotDecOne),
    (SY:   60; SM:  4; SD: 19; EY:   60; EM:  9; ED: 13; DayOffset: lotDecOne),
    (SY:   60; SM:  9; SD: 14; EY:   60; EM: 10; ED: 12; DayOffset: lotDecTwo),
    (SY:   60; SM: 10; SD: 13; EY:   60; EM: 12; ED: 11; DayOffset: lotDecOne),
    (SY:   61; SM:  1; SD: 10; EY:   61; EM:  2; ED:  8; DayOffset: lotDecOne),
    (SY:   61; SM:  3; SD: 10; EY:   61; EM:  4; ED:  8; DayOffset: lotDecOne),
    (SY:   61; SM:  5; SD:  8; EY:   61; EM:  9; ED:  2; DayOffset: lotDecOne),
    (SY:   61; SM:  9; SD:  3; EY:   61; EM: 10; ED:  1; DayOffset: lotDecTwo),
    (SY:   61; SM: 10; SD:  2; EY:   61; EM: 11; ED: 30; DayOffset: lotDecOne),
    (SY:   61; SM: 12; SD: 30; EY:   62; EM:  2; ED: 27; DayOffset: lotDecOne),
    (SY:   62; SM:  3; SD: 29; EY:   62; EM:  4; ED: 27; DayOffset: lotDecOne),
    (SY:   62; SM:  5; SD: 27; EY:   62; EM:  9; ED: 21; DayOffset: lotDecOne),
    (SY:   62; SM:  9; SD: 22; EY:   62; EM: 10; ED: 20; DayOffset: lotDecTwo),
    (SY:   62; SM: 10; SD: 21; EY:   63; EM:  2; ED: 16; DayOffset: lotDecOne),
    (SY:   63; SM:  6; SD: 15; EY:   63; EM: 10; ED: 10; DayOffset: lotDecOne),
    (SY:   63; SM: 10; SD: 11; EY:   63; EM: 11; ED:  8; DayOffset: lotDecTwo),
    (SY:   63; SM: 11; SD:  9; EY:   64; EM:  3; ED:  6; DayOffset: lotDecOne),
    (SY:   64; SM:  4; SD:  5; EY:   64; EM:  5; ED:  4; DayOffset: lotDecOne),
    (SY:   64; SM:  6; SD:  3; EY:   64; EM:  7; ED:  2; DayOffset: lotDecOne),
    (SY:   64; SM:  8; SD:  1; EY:   64; EM: 10; ED: 28; DayOffset: lotDecOne),
    (SY:   64; SM: 10; SD: 29; EY:   64; EM: 11; ED: 26; DayOffset: lotDecTwo),
    (SY:   64; SM: 11; SD: 27; EY:   64; EM: 12; ED: 26; DayOffset: lotDecOne),
    (SY:   64; SM: 12; SD: 27; EY:   65; EM:  1; ED: 24; DayOffset: lotDecTwo),
    (SY:   65; SM:  1; SD: 25; EY:   65; EM:  5; ED: 23; DayOffset: lotDecOne),
    (SY:   65; SM:  6; SD: 22; EY:   65; EM:  7; ED: 21; DayOffset: lotDecOne),
    (SY:   65; SM:  8; SD: 20; EY:   65; EM:  9; ED: 18; DayOffset: lotDecOne),
    (SY:   65; SM: 10; SD: 18; EY:   66; EM:  1; ED: 14; DayOffset: lotDecOne),
    (SY:   66; SM:  1; SD: 15; EY:   66; EM:  2; ED: 12; DayOffset: lotDecTwo),
    (SY:   66; SM:  2; SD: 13; EY:   66; EM:  8; ED:  9; DayOffset: lotDecOne),
    (SY:   66; SM:  9; SD:  8; EY:   66; EM: 10; ED:  7; DayOffset: lotDecOne),
    (SY:   66; SM: 11; SD:  6; EY:   66; EM: 12; ED:  5; DayOffset: lotDecOne),
    (SY:   67; SM:  1; SD:  4; EY:   67; EM:  2; ED:  2; DayOffset: lotDecOne),
    (SY:   67; SM:  3; SD:  4; EY:   67; EM:  4; ED:  2; DayOffset: lotDecOne),
    (SY:   67; SM:  4; SD:  3; EY:   67; EM:  5; ED:  1; DayOffset: lotDecTwo),
    (SY:   67; SM:  5; SD:  2; EY:   67; EM:  5; ED: 31; DayOffset: lotDecOne),
    (SY:   67; SM:  6; SD:  1; EY:   67; EM:  6; ED: 29; DayOffset: lotDecTwo),
    (SY:   67; SM:  6; SD: 30; EY:   67; EM: 10; ED: 26; DayOffset: lotDecOne),
    (SY:   67; SM: 11; SD: 25; EY:   67; EM: 12; ED: 24; DayOffset: lotDecOne),
    (SY:   68; SM:  1; SD: 23; EY:   68; EM:  2; ED: 21; DayOffset: lotDecOne),
    (SY:   68; SM:  3; SD: 22; EY:   68; EM:  8; ED: 16; DayOffset: lotDecOne),
    (SY:   68; SM:  8; SD: 17; EY:   68; EM:  9; ED: 14; DayOffset: lotDecTwo),
    (SY:   68; SM:  9; SD: 15; EY:   68; EM: 11; ED: 13; DayOffset: lotDecOne),
    (SY:   68; SM: 12; SD: 13; EY:   69; EM:  1; ED: 11; DayOffset: lotDecOne),
    (SY:   69; SM:  2; SD: 10; EY:   69; EM:  3; ED: 11; DayOffset: lotDecOne),
    (SY:   69; SM:  4; SD: 10; EY:   69; EM: 11; ED:  2; DayOffset: lotDecOne),
    (SY:   69; SM: 12; SD:  2; EY:   70; EM:  1; ED: 30; DayOffset: lotDecOne),
    (SY:   70; SM:  3; SD:  1; EY:   70; EM:  3; ED: 30; DayOffset: lotDecOne),
    (SY:   70; SM:  4; SD: 29; EY:   70; EM:  8; ED: 24; DayOffset: lotDecOne),
    (SY:   70; SM:  8; SD: 25; EY:   70; EM:  9; ED: 22; DayOffset: lotDecTwo),
    (SY:   70; SM:  9; SD: 23; EY:   71; EM:  1; ED: 19; DayOffset: lotDecOne),
    (SY:   71; SM:  3; SD: 20; EY:   71; EM:  4; ED: 18; DayOffset: lotDecOne),
    (SY:   71; SM:  5; SD: 18; EY:   71; EM:  9; ED: 12; DayOffset: lotDecOne),
    (SY:   71; SM:  9; SD: 13; EY:   71; EM: 10; ED: 11; DayOffset: lotDecTwo),
    (SY:   71; SM: 10; SD: 12; EY:   71; EM: 11; ED: 10; DayOffset: lotDecOne),
    (SY:   71; SM: 11; SD: 11; EY:   71; EM: 12; ED:  9; DayOffset: lotDecTwo),
    (SY:   71; SM: 12; SD: 10; EY:   72; EM:  2; ED:  7; DayOffset: lotDecOne),
    (SY:   72; SM:  7; SD:  4; EY:   72; EM:  9; ED: 30; DayOffset: lotDecOne),
    (SY:   72; SM: 10; SD:  1; EY:   72; EM: 10; ED: 29; DayOffset: lotDecTwo),
    (SY:   72; SM: 10; SD: 30; EY:   72; EM: 11; ED: 28; DayOffset: lotDecOne),
    (SY:   72; SM: 11; SD: 29; EY:   72; EM: 12; ED: 27; DayOffset: lotDecTwo),
    (SY:   72; SM: 12; SD: 28; EY:   73; EM:  2; ED: 25; DayOffset: lotDecOne),
    (SY:   73; SM:  3; SD: 27; EY:   73; EM:  4; ED: 25; DayOffset: lotDecOne),
    (SY:   73; SM:  5; SD: 25; EY:   73; EM:  6; ED: 23; DayOffset: lotDecOne),
    (SY:   73; SM:  7; SD: 23; EY:   73; EM:  8; ED: 21; DayOffset: lotDecOne),
    (SY:   73; SM:  9; SD: 20; EY:   73; EM: 10; ED: 19; DayOffset: lotDecOne),
    (SY:   73; SM: 10; SD: 20; EY:   73; EM: 11; ED: 17; DayOffset: lotDecTwo),
    (SY:   73; SM: 11; SD: 18; EY:   73; EM: 12; ED: 17; DayOffset: lotDecOne),
    (SY:   73; SM: 12; SD: 18; EY:   74; EM:  1; ED: 15; DayOffset: lotDecTwo),
    (SY:   74; SM:  1; SD: 16; EY:   74; EM:  5; ED: 14; DayOffset: lotDecOne),
    (SY:   74; SM:  6; SD: 13; EY:   74; EM:  7; ED: 12; DayOffset: lotDecOne),
    (SY:   74; SM:  8; SD: 11; EY:   74; EM:  9; ED:  9; DayOffset: lotDecOne),
    (SY:   74; SM: 10; SD:  9; EY:   75; EM:  3; ED:  5; DayOffset: lotDecOne),
    (SY:   75; SM:  3; SD:  6; EY:   75; EM:  4; ED:  3; DayOffset: lotDecTwo),
    (SY:   75; SM:  4; SD:  4; EY:   75; EM:  9; ED: 28; DayOffset: lotDecOne),
    (SY:   75; SM: 10; SD: 28; EY:   75; EM: 11; ED: 26; DayOffset: lotDecOne),
    (SY:   75; SM: 12; SD: 26; EY:   76; EM: 10; ED: 16; DayOffset: lotDecOne),
    (SY:   76; SM: 11; SD: 15; EY:   76; EM: 12; ED: 14; DayOffset: lotDecOne),
    (SY:   77; SM:  1; SD: 13; EY:   77; EM:  2; ED: 11; DayOffset: lotDecOne),
    (SY:   77; SM:  3; SD: 13; EY:   78; EM:  1; ED:  2; DayOffset: lotDecOne),
    (SY:   78; SM:  2; SD:  1; EY:   78; EM:  3; ED:  2; DayOffset: lotDecOne),
    (SY:   78; SM:  4; SD:  1; EY:   78; EM:  7; ED: 27; DayOffset: lotDecOne),
    (SY:   78; SM:  7; SD: 28; EY:   78; EM:  8; ED: 25; DayOffset: lotDecTwo),
    (SY:   78; SM:  8; SD: 26; EY:   78; EM: 12; ED: 22; DayOffset: lotDecOne),
    (SY:   79; SM:  2; SD: 20; EY:   79; EM:  3; ED: 21; DayOffset: lotDecOne),
    (SY:   79; SM:  4; SD: 20; EY:   79; EM:  8; ED: 15; DayOffset: lotDecOne),
    (SY:   79; SM:  8; SD: 16; EY:   79; EM:  9; ED: 13; DayOffset: lotDecTwo),
    (SY:   79; SM:  9; SD: 14; EY:   79; EM: 10; ED: 13; DayOffset: lotDecOne),
    (SY:   79; SM: 10; SD: 14; EY:   79; EM: 11; ED: 11; DayOffset: lotDecTwo),
    (SY:   79; SM: 11; SD: 12; EY:   80; EM:  1; ED: 10; DayOffset: lotDecOne),
    (SY:   80; SM:  5; SD:  8; EY:   80; EM:  9; ED:  2; DayOffset: lotDecOne),
    (SY:   80; SM:  9; SD:  3; EY:   80; EM: 10; ED:  1; DayOffset: lotDecTwo),
    (SY:   80; SM: 10; SD:  2; EY:   80; EM: 10; ED: 31; DayOffset: lotDecOne),
    (SY:   80; SM: 11; SD:  1; EY:   80; EM: 11; ED: 29; DayOffset: lotDecTwo),
    (SY:   80; SM: 11; SD: 30; EY:   81; EM:  1; ED: 28; DayOffset: lotDecOne),
    (SY:   81; SM:  2; SD: 27; EY:   81; EM:  3; ED: 28; DayOffset: lotDecOne),
    (SY:   81; SM:  6; SD: 25; EY:   81; EM:  7; ED: 24; DayOffset: lotDecOne),
    (SY:   81; SM:  8; SD: 23; EY:   81; EM:  9; ED: 21; DayOffset: lotDecOne),
    (SY:   81; SM:  9; SD: 22; EY:   81; EM: 10; ED: 20; DayOffset: lotDecTwo),
    (SY:   81; SM: 10; SD: 21; EY:   81; EM: 11; ED: 19; DayOffset: lotDecOne),
    (SY:   81; SM: 11; SD: 20; EY:   81; EM: 12; ED: 18; DayOffset: lotDecTwo),
    (SY:   81; SM: 12; SD: 19; EY:   82; EM:  4; ED: 16; DayOffset: lotDecOne),
    (SY:   82; SM:  5; SD: 16; EY:   82; EM:  6; ED: 14; DayOffset: lotDecOne),
    (SY:   82; SM:  7; SD: 14; EY:   82; EM:  8; ED: 12; DayOffset: lotDecOne),
    (SY:   82; SM:  9; SD: 11; EY:   83; EM:  2; ED:  5; DayOffset: lotDecOne),
    (SY:   83; SM:  2; SD:  6; EY:   83; EM:  3; ED:  6; DayOffset: lotDecTwo),
    (SY:   83; SM:  3; SD:  7; EY:   83; EM:  7; ED:  3; DayOffset: lotDecOne),
    (SY:   83; SM:  8; SD:  2; EY:   83; EM:  8; ED: 31; DayOffset: lotDecOne),
    (SY:   83; SM:  9; SD: 30; EY:   84; EM:  4; ED: 22; DayOffset: lotDecOne),
    (SY:   84; SM:  4; SD: 23; EY:   84; EM:  5; ED: 23; DayOffset: lotDecTwo),
    (SY:   84; SM:  5; SD: 24; EY:   84; EM: 11; ED: 16; DayOffset: lotDecOne),
    (SY:   84; SM: 12; SD: 16; EY:   85; EM:  2; ED: 12; DayOffset: lotDecOne),
    (SY:   85; SM:  3; SD: 14; EY:   85; EM:  4; ED: 12; DayOffset: lotDecOne),
    (SY:   85; SM:  5; SD: 12; EY:   85; EM:  6; ED: 10; DayOffset: lotDecOne),
    (SY:   85; SM:  7; SD: 10; EY:   85; EM:  8; ED:  8; DayOffset: lotDecOne),
    (SY:   85; SM: 12; SD:  5; EY:   86; EM:  1; ED:  3; DayOffset: lotIncOne),
    (SY:   86; SM:  2; SD:  2; EY:   86; EM:  3; ED:  3; DayOffset: lotIncOne),
    (SY:   86; SM:  5; SD: 31; EY:   86; EM:  8; ED: 27; DayOffset: lotDecOne),
    (SY:   86; SM:  9; SD: 26; EY:   86; EM: 10; ED: 25; DayOffset: lotDecOne),
    (SY:   86; SM: 12; SD: 24; EY:   87; EM:  1; ED: 22; DayOffset: lotIncOne),
    (SY:   87; SM:  2; SD: 21; EY:   87; EM:  3; ED: 22; DayOffset: lotIncOne),
    (SY:   87; SM:  7; SD: 18; EY:   87; EM:  8; ED: 16; DayOffset: lotDecOne),
    (SY:   87; SM:  9; SD: 15; EY:   87; EM: 11; ED: 13; DayOffset: lotDecOne),
    (SY:   88; SM:  1; SD: 12; EY:   88; EM:  2; ED: 10; DayOffset: lotIncOne),
    (SY:   88; SM:  3; SD: 11; EY:   88; EM:  4; ED:  9; DayOffset: lotIncOne),
    (SY:   88; SM:  8; SD:  5; EY:   88; EM:  9; ED:  3; DayOffset: lotDecOne),
    (SY:   88; SM: 10; SD:  3; EY:   88; EM: 11; ED:  1; DayOffset: lotDecOne),
    (SY:   89; SM:  3; SD: 30; EY:   89; EM:  4; ED: 28; DayOffset: lotIncOne),
    (SY:   89; SM:  8; SD: 24; EY:   89; EM:  9; ED: 22; DayOffset: lotDecOne),
    (SY:   89; SM: 10; SD: 22; EY:   89; EM: 11; ED: 20; DayOffset: lotDecOne),
    (SY:   90; SM:  3; SD: 19; EY:   90; EM:  4; ED: 17; DayOffset: lotIncOne),
    (SY:   90; SM:  9; SD: 12; EY:   90; EM: 10; ED: 11; DayOffset: lotDecOne),
    (SY:   90; SM: 11; SD: 10; EY:   90; EM: 12; ED:  9; DayOffset: lotDecOne),
    (SY:   91; SM:  1; SD:  8; EY:   91; EM:  2; ED:  6; DayOffset: lotDecOne),
    (SY:   91; SM:  4; SD:  7; EY:   91; EM:  5; ED:  6; DayOffset: lotIncOne),
    (SY:   91; SM:  6; SD:  5; EY:   91; EM:  7; ED:  4; DayOffset: lotIncOne),
    (SY:   91; SM: 10; SD:  1; EY:   91; EM: 10; ED: 30; DayOffset: lotDecOne),
    (SY:   91; SM: 11; SD: 29; EY:   91; EM: 12; ED: 28; DayOffset: lotDecOne),
    (SY:   92; SM:  1; SD: 27; EY:   92; EM:  2; ED: 25; DayOffset: lotDecOne),
    (SY:   92; SM: 12; SD: 17; EY:   93; EM:  1; ED: 15; DayOffset: lotDecOne),
    (SY:   93; SM:  2; SD: 14; EY:   93; EM:  3; ED: 15; DayOffset: lotDecOne),
    (SY:   93; SM:  4; SD: 14; EY:   93; EM:  5; ED: 13; DayOffset: lotDecOne),
    (SY:   93; SM:  6; SD: 12; EY:   93; EM:  7; ED: 11; DayOffset: lotDecOne),
    (SY:   94; SM:  5; SD:  3; EY:   94; EM:  6; ED:  1; DayOffset: lotDecOne),
    (SY:   94; SM:  7; SD:  1; EY:   94; EM:  7; ED: 30; DayOffset: lotDecOne),
    (SY:   95; SM:  6; SD: 20; EY:   95; EM:  8; ED: 18; DayOffset: lotDecOne),
    (SY:   95; SM:  9; SD: 17; EY:   95; EM: 10; ED: 16; DayOffset: lotDecOne),
    (SY:   96; SM:  2; SD: 12; EY:   96; EM:  3; ED: 12; DayOffset: lotIncOne),
    (SY:   96; SM:  7; SD:  8; EY:   96; EM:  8; ED:  6; DayOffset: lotDecOne),
    (SY:   96; SM:  9; SD:  5; EY:   96; EM: 10; ED:  4; DayOffset: lotDecOne),
    (SY:   97; SM:  3; SD:  2; EY:   97; EM:  3; ED: 31; DayOffset: lotIncOne),
    (SY:   97; SM:  7; SD: 27; EY:   97; EM:  8; ED: 25; DayOffset: lotDecOne),
    (SY:   97; SM:  9; SD: 24; EY:   97; EM: 10; ED: 23; DayOffset: lotDecOne),
    (SY:   98; SM:  8; SD: 15; EY:   98; EM:  9; ED: 13; DayOffset: lotDecOne),
    (SY:   98; SM: 10; SD: 13; EY:   98; EM: 11; ED: 11; DayOffset: lotDecOne),
    (SY:   98; SM: 12; SD: 11; EY:   99; EM:  1; ED:  9; DayOffset: lotDecOne),
    (SY:   99; SM:  3; SD: 10; EY:   99; EM:  4; ED:  8; DayOffset: lotIncOne),
    (SY:   99; SM:  5; SD:  8; EY:   99; EM:  6; ED:  6; DayOffset: lotIncOne),
    (SY:   99; SM:  9; SD:  3; EY:   99; EM: 10; ED:  2; DayOffset: lotDecOne),
    (SY:   99; SM: 11; SD:  1; EY:   99; EM: 11; ED: 30; DayOffset: lotDecOne),
    (SY:   99; SM: 12; SD: 30; EY:  100; EM:  1; ED: 28; DayOffset: lotDecOne),
    (SY:  100; SM:  5; SD: 26; EY:  100; EM:  6; ED: 24; DayOffset: lotIncOne),
    (SY:  100; SM: 11; SD: 19; EY:  100; EM: 12; ED: 18; DayOffset: lotDecOne),
    (SY:  101; SM:  1; SD: 17; EY:  101; EM:  2; ED: 15; DayOffset: lotDecOne),
    (SY:  101; SM:  3; SD: 17; EY:  101; EM:  4; ED: 15; DayOffset: lotDecOne),
    (SY:  102; SM:  2; SD:  5; EY:  102; EM:  3; ED:  6; DayOffset: lotDecOne),
    (SY:  102; SM:  4; SD:  5; EY:  102; EM:  5; ED:  4; DayOffset: lotDecOne),
    (SY:  102; SM:  6; SD:  3; EY:  102; EM:  7; ED:  2; DayOffset: lotDecOne),
    (SY:  103; SM:  5; SD: 23; EY:  103; EM:  7; ED: 21; DayOffset: lotDecOne),
    (SY:  103; SM:  8; SD: 20; EY:  103; EM:  9; ED: 18; DayOffset: lotDecOne),
    (SY:  104; SM:  6; SD: 10; EY:  104; EM:  7; ED:  9; DayOffset: lotDecOne),
    (SY:  104; SM:  8; SD:  8; EY:  104; EM: 10; ED:  6; DayOffset: lotDecOne),
    (SY:  105; SM:  6; SD: 29; EY:  105; EM:  7; ED: 28; DayOffset: lotDecOne),
    (SY:  105; SM:  8; SD: 27; EY:  105; EM:  9; ED: 25; DayOffset: lotDecOne),
    (SY:  106; SM:  7; SD: 18; EY:  106; EM:  8; ED: 16; DayOffset: lotDecOne),
    (SY:  106; SM:  9; SD: 15; EY:  106; EM: 10; ED: 14; DayOffset: lotDecOne),
    (SY:  106; SM: 11; SD: 13; EY:  106; EM: 12; ED: 12; DayOffset: lotDecOne),
    (SY:  107; SM:  2; SD: 10; EY:  107; EM:  3; ED: 11; DayOffset: lotIncOne),
    (SY:  107; SM:  4; SD: 10; EY:  107; EM:  5; ED:  9; DayOffset: lotIncOne),
    (SY:  107; SM:  8; SD:  6; EY:  107; EM:  9; ED:  4; DayOffset: lotDecOne),
    (SY:  107; SM: 10; SD:  4; EY:  107; EM: 11; ED:  2; DayOffset: lotDecOne),
    (SY:  107; SM: 12; SD:  2; EY:  107; EM: 12; ED: 31; DayOffset: lotDecOne),
    (SY:  108; SM:  4; SD: 28; EY:  108; EM:  5; ED: 27; DayOffset: lotIncOne),
    (SY:  108; SM:  6; SD: 26; EY:  108; EM:  7; ED: 25; DayOffset: lotIncOne),
    (SY:  108; SM:  8; SD: 24; EY:  108; EM:  9; ED: 22; DayOffset: lotDecOne),
    (SY:  108; SM: 10; SD: 22; EY:  108; EM: 11; ED: 20; DayOffset: lotDecOne),
    (SY:  108; SM: 12; SD: 20; EY:  109; EM:  1; ED: 18; DayOffset: lotDecOne),
    (SY:  109; SM:  2; SD: 17; EY:  109; EM:  3; ED: 18; DayOffset: lotDecOne),
    (SY:  110; SM:  1; SD:  8; EY:  110; EM:  2; ED:  6; DayOffset: lotDecOne),
    (SY:  110; SM:  3; SD:  8; EY:  110; EM:  4; ED:  6; DayOffset: lotDecOne),
    (SY:  110; SM:  5; SD:  6; EY:  110; EM:  6; ED:  4; DayOffset: lotDecOne),
    (SY:  111; SM:  3; SD: 27; EY:  111; EM:  4; ED: 25; DayOffset: lotDecOne),
    (SY:  111; SM:  5; SD: 25; EY:  111; EM:  6; ED: 23; DayOffset: lotDecOne),
    (SY:  111; SM:  7; SD: 23; EY:  111; EM:  8; ED: 21; DayOffset: lotDecOne),
    (SY:  112; SM:  5; SD: 13; EY:  112; EM:  6; ED: 11; DayOffset: lotDecOne),
    (SY:  112; SM:  7; SD: 11; EY:  112; EM:  9; ED:  8; DayOffset: lotDecOne),
    (SY:  113; SM:  6; SD:  1; EY:  113; EM:  6; ED: 30; DayOffset: lotDecOne),
    (SY:  113; SM:  7; SD: 30; EY:  113; EM:  8; ED: 28; DayOffset: lotDecOne),
    (SY:  114; SM:  6; SD: 20; EY:  114; EM:  7; ED: 19; DayOffset: lotDecOne),
    (SY:  114; SM:  8; SD: 18; EY:  114; EM:  9; ED: 16; DayOffset: lotDecOne),
    (SY:  114; SM: 10; SD: 16; EY:  114; EM: 11; ED: 14; DayOffset: lotDecOne),
    (SY:  115; SM:  1; SD: 13; EY:  115; EM:  2; ED: 11; DayOffset: lotIncOne),
    (SY:  115; SM:  7; SD:  9; EY:  115; EM: 12; ED:  3; DayOffset: lotDecOne),
    (SY:  116; SM:  3; SD: 31; EY:  116; EM:  4; ED: 29; DayOffset: lotIncOne),
    (SY:  116; SM:  5; SD: 29; EY:  116; EM:  6; ED: 27; DayOffset: lotIncOne),
    (SY:  116; SM:  7; SD: 27; EY:  116; EM:  8; ED: 25; DayOffset: lotDecOne),
    (SY:  116; SM:  9; SD: 24; EY:  116; EM: 12; ED: 21; DayOffset: lotDecOne),
    (SY:  117; SM:  1; SD: 20; EY:  117; EM:  2; ED: 18; DayOffset: lotDecOne),
    (SY:  117; SM:  4; SD: 19; EY:  117; EM:  5; ED: 18; DayOffset: lotIncOne),
    (SY:  117; SM:  6; SD: 17; EY:  117; EM:  7; ED: 16; DayOffset: lotIncOne),
    (SY:  117; SM: 10; SD: 13; EY:  118; EM:  1; ED:  9; DayOffset: lotDecOne),
    (SY:  118; SM:  2; SD:  8; EY:  118; EM:  3; ED:  9; DayOffset: lotDecOne),
    (SY:  119; SM:  2; SD: 27; EY:  119; EM:  3; ED: 28; DayOffset: lotDecOne),
    (SY:  119; SM:  4; SD: 27; EY:  119; EM:  5; ED: 26; DayOffset: lotDecOne),
    (SY:  120; SM:  4; SD: 15; EY:  120; EM:  5; ED: 14; DayOffset: lotDecOne),
    (SY:  120; SM:  7; SD: 13; EY:  120; EM:  8; ED: 11; DayOffset: lotDecOne),
    (SY:  121; SM:  5; SD:  4; EY:  121; EM:  6; ED:  2; DayOffset: lotDecOne),
    (SY:  121; SM:  7; SD:  2; EY:  121; EM:  7; ED: 31; DayOffset: lotDecOne),
    (SY:  121; SM:  9; SD: 29; EY:  121; EM: 10; ED: 28; DayOffset: lotDecOne),
    (SY:  122; SM:  5; SD: 23; EY:  122; EM:  6; ED: 21; DayOffset: lotDecOne),
    (SY:  122; SM:  7; SD: 21; EY:  122; EM:  8; ED: 19; DayOffset: lotDecOne),
    (SY:  122; SM:  9; SD: 18; EY:  122; EM: 10; ED: 17; DayOffset: lotDecOne),
    (SY:  122; SM: 12; SD: 16; EY:  123; EM:  1; ED: 14; DayOffset: lotIncOne),
    (SY:  123; SM:  6; SD: 11; EY:  123; EM:  7; ED: 10; DayOffset: lotDecOne),
    (SY:  123; SM:  8; SD:  9; EY:  123; EM: 11; ED:  5; DayOffset: lotDecOne),
    (SY:  124; SM:  3; SD:  3; EY:  124; EM:  4; ED:  1; DayOffset: lotIncOne),
    (SY:  124; SM:  6; SD: 29; EY:  124; EM: 11; ED: 23; DayOffset: lotDecOne),
    (SY:  124; SM: 12; SD: 23; EY:  125; EM:  1; ED: 21; DayOffset: lotDecOne),
    (SY:  125; SM:  3; SD: 22; EY:  125; EM:  4; ED: 20; DayOffset: lotIncOne),
    (SY:  125; SM:  5; SD: 20; EY:  125; EM:  6; ED: 18; DayOffset: lotIncOne),
    (SY:  125; SM:  8; SD: 16; EY:  125; EM: 12; ED: 12; DayOffset: lotDecOne),
    (SY:  126; SM:  1; SD: 11; EY:  126; EM:  2; ED:  9; DayOffset: lotDecOne),
    (SY:  126; SM: 11; SD:  2; EY:  126; EM: 12; ED:  1; DayOffset: lotDecOne),
    (SY:  126; SM: 12; SD: 31; EY:  127; EM:  2; ED: 28; DayOffset: lotDecOne),
    (SY:  127; SM:  3; SD: 30; EY:  127; EM:  4; ED: 28; DayOffset: lotDecOne),
    (SY:  127; SM: 11; SD: 21; EY:  127; EM: 12; ED: 20; DayOffset: lotDecOne),
    (SY:  128; SM:  1; SD: 19; EY:  128; EM:  2; ED: 17; DayOffset: lotDecOne),
    (SY:  128; SM:  4; SD: 17; EY:  128; EM:  5; ED: 16; DayOffset: lotDecOne),
    (SY:  128; SM:  6; SD: 15; EY:  128; EM:  7; ED: 14; DayOffset: lotDecOne),
    (SY:  129; SM:  2; SD:  6; EY:  129; EM:  3; ED:  7; DayOffset: lotDecOne),
    (SY:  129; SM:  4; SD:  6; EY:  129; EM:  5; ED:  5; DayOffset: lotDecOne),
    (SY:  129; SM:  6; SD:  4; EY:  129; EM:  7; ED:  3; DayOffset: lotDecOne),
    (SY:  129; SM:  9; SD:  1; EY:  129; EM:  9; ED: 30; DayOffset: lotDecOne),
    (SY:  130; SM:  4; SD: 25; EY:  130; EM:  5; ED: 24; DayOffset: lotDecOne),
    (SY:  130; SM:  6; SD: 23; EY:  130; EM:  7; ED: 22; DayOffset: lotDecOne),
    (SY:  130; SM:  8; SD: 21; EY:  130; EM:  9; ED: 19; DayOffset: lotDecOne),
    (SY:  131; SM:  5; SD: 14; EY:  131; EM:  6; ED: 12; DayOffset: lotDecOne),
    (SY:  131; SM:  7; SD: 12; EY:  131; EM: 10; ED:  8; DayOffset: lotDecOne),
    (SY:  132; SM:  2; SD:  4; EY:  132; EM:  3; ED:  4; DayOffset: lotIncOne),
    (SY:  132; SM:  6; SD:  1; EY:  132; EM: 10; ED: 26; DayOffset: lotDecOne),
    (SY:  132; SM: 11; SD: 25; EY:  132; EM: 12; ED: 24; DayOffset: lotDecOne),
    (SY:  133; SM:  2; SD: 22; EY:  133; EM:  3; ED: 23; DayOffset: lotIncOne),
    (SY:  133; SM:  4; SD: 22; EY:  133; EM:  5; ED: 21; DayOffset: lotIncOne),
    (SY:  133; SM:  7; SD: 19; EY:  134; EM:  1; ED: 12; DayOffset: lotDecOne),
    (SY:  134; SM:  5; SD: 11; EY:  134; EM:  6; ED:  9; DayOffset: lotIncOne),
    (SY:  134; SM: 10; SD:  5; EY:  135; EM:  1; ED: 31; DayOffset: lotDecOne),
    (SY:  135; SM: 10; SD: 24; EY:  135; EM: 11; ED: 22; DayOffset: lotDecOne),
    (SY:  135; SM: 12; SD: 22; EY:  136; EM:  1; ED: 20; DayOffset: lotDecOne),
    (SY:  136; SM:  3; SD: 20; EY:  136; EM:  4; ED: 18; DayOffset: lotDecOne),
    (SY:  136; SM: 11; SD: 11; EY:  136; EM: 12; ED: 10; DayOffset: lotDecOne),
    (SY:  137; SM:  1; SD:  9; EY:  137; EM:  2; ED:  7; DayOffset: lotDecOne),
    (SY:  137; SM:  3; SD:  9; EY:  137; EM:  4; ED:  7; DayOffset: lotDecOne),
    (SY:  137; SM:  6; SD:  6; EY:  137; EM:  7; ED:  5; DayOffset: lotDecOne),
    (SY:  137; SM:  8; SD:  4; EY:  137; EM:  9; ED:  2; DayOffset: lotDecOne),
    (SY:  138; SM:  1; SD: 28; EY:  138; EM:  2; ED: 26; DayOffset: lotDecOne),
    (SY:  138; SM:  3; SD: 28; EY:  138; EM:  4; ED: 26; DayOffset: lotDecOne),
    (SY:  138; SM:  5; SD: 26; EY:  138; EM:  6; ED: 24; DayOffset: lotDecOne),
    (SY:  139; SM:  4; SD: 16; EY:  139; EM:  5; ED: 15; DayOffset: lotDecOne),
    (SY:  139; SM:  6; SD: 14; EY:  139; EM:  7; ED: 13; DayOffset: lotDecOne),
    (SY:  139; SM:  8; SD: 12; EY:  139; EM:  9; ED: 10; DayOffset: lotDecOne),
    (SY:  140; SM:  1; SD:  7; EY:  140; EM:  2; ED:  5; DayOffset: lotIncOne),
    (SY:  140; SM:  5; SD:  4; EY:  140; EM:  6; ED:  2; DayOffset: lotDecOne),
    (SY:  140; SM:  7; SD:  2; EY:  140; EM:  9; ED: 28; DayOffset: lotDecOne),
    (SY:  140; SM: 10; SD: 28; EY:  140; EM: 11; ED: 26; DayOffset: lotDecOne),
    (SY:  141; SM:  1; SD: 25; EY:  141; EM:  2; ED: 23; DayOffset: lotIncOne),
    (SY:  141; SM:  3; SD: 25; EY:  141; EM:  4; ED: 23; DayOffset: lotIncOne),
    (SY:  141; SM:  5; SD: 23; EY:  141; EM: 12; ED: 15; DayOffset: lotDecOne),
    (SY:  142; SM:  2; SD: 13; EY:  142; EM:  3; ED: 14; DayOffset: lotIncOne),
    (SY:  142; SM:  4; SD: 13; EY:  142; EM:  5; ED: 12; DayOffset: lotIncOne),
    (SY:  142; SM:  9; SD:  7; EY:  143; EM:  1; ED:  3; DayOffset: lotDecOne),
    (SY:  143; SM:  9; SD: 26; EY:  143; EM: 10; ED: 25; DayOffset: lotDecOne),
    (SY:  143; SM: 11; SD: 24; EY:  144; EM:  1; ED: 22; DayOffset: lotDecOne),
    (SY:  144; SM:  2; SD: 21; EY:  144; EM:  3; ED: 21; DayOffset: lotDecOne),
    (SY:  144; SM: 10; SD: 14; EY:  144; EM: 11; ED: 12; DayOffset: lotDecOne),
    (SY:  144; SM: 12; SD: 12; EY:  145; EM:  1; ED: 10; DayOffset: lotDecOne),
    (SY:  145; SM:  2; SD:  9; EY:  145; EM:  3; ED: 10; DayOffset: lotDecOne),
    (SY:  145; SM:  5; SD:  9; EY:  145; EM:  6; ED:  7; DayOffset: lotDecOne),
    (SY:  145; SM: 11; SD:  2; EY:  145; EM: 12; ED:  1; DayOffset: lotDecOne),
    (SY:  145; SM: 12; SD: 31; EY:  146; EM:  1; ED: 29; DayOffset: lotDecOne),
    (SY:  146; SM:  2; SD: 28; EY:  146; EM:  3; ED: 29; DayOffset: lotDecOne),
    (SY:  146; SM:  4; SD: 28; EY:  146; EM:  5; ED: 27; DayOffset: lotDecOne),
    (SY:  146; SM:  9; SD: 23; EY:  146; EM: 10; ED: 22; DayOffset: lotDecOne),
    (SY:  146; SM: 11; SD: 21; EY:  146; EM: 12; ED: 20; DayOffset: lotDecOne),
    (SY:  147; SM:  1; SD: 19; EY:  147; EM:  2; ED: 17; DayOffset: lotDecOne),
    (SY:  147; SM:  3; SD: 19; EY:  147; EM:  4; ED: 17; DayOffset: lotDecOne),
    (SY:  147; SM:  5; SD: 17; EY:  147; EM:  6; ED: 15; DayOffset: lotDecOne),
    (SY:  147; SM:  7; SD: 15; EY:  147; EM:  8; ED: 13; DayOffset: lotDecOne),
    (SY:  148; SM:  2; SD:  7; EY:  148; EM:  3; ED:  7; DayOffset: lotDecOne),
    (SY:  148; SM:  4; SD:  6; EY:  148; EM:  5; ED:  5; DayOffset: lotDecOne),
    (SY:  148; SM:  6; SD:  4; EY:  148; EM:  8; ED: 31; DayOffset: lotDecOne),
    (SY:  148; SM:  9; SD: 30; EY:  148; EM: 10; ED: 29; DayOffset: lotDecOne),
    (SY:  148; SM: 12; SD: 28; EY:  149; EM:  1; ED: 26; DayOffset: lotIncOne),
    (SY:  149; SM:  2; SD: 25; EY:  149; EM:  3; ED: 26; DayOffset: lotIncOne),
    (SY:  149; SM:  4; SD: 25; EY:  149; EM:  5; ED: 24; DayOffset: lotDecOne),
    (SY:  149; SM:  6; SD: 23; EY:  149; EM: 11; ED: 17; DayOffset: lotDecOne),
    (SY:  150; SM:  1; SD: 16; EY:  150; EM:  2; ED: 14; DayOffset: lotIncOne),
    (SY:  150; SM:  3; SD: 16; EY:  150; EM:  4; ED: 14; DayOffset: lotIncOne),
    (SY:  150; SM:  6; SD: 12; EY:  150; EM:  7; ED: 11; DayOffset: lotDecOne),
    (SY:  150; SM:  8; SD: 10; EY:  150; EM: 12; ED:  6; DayOffset: lotDecOne),
    (SY:  151; SM:  8; SD: 29; EY:  151; EM:  9; ED: 27; DayOffset: lotDecOne),
    (SY:  151; SM: 10; SD: 27; EY:  151; EM: 12; ED: 25; DayOffset: lotDecOne),
    (SY:  152; SM:  1; SD: 24; EY:  152; EM:  2; ED: 22; DayOffset: lotDecOne),
    (SY:  152; SM:  9; SD: 16; EY:  152; EM: 10; ED: 15; DayOffset: lotDecOne),
    (SY:  152; SM: 11; SD: 14; EY:  152; EM: 12; ED: 13; DayOffset: lotDecOne),
    (SY:  153; SM:  1; SD: 12; EY:  153; EM:  2; ED: 10; DayOffset: lotDecOne),
    (SY:  153; SM:  8; SD:  7; EY:  153; EM:  9; ED:  5; DayOffset: lotDecOne),
    (SY:  153; SM: 10; SD:  5; EY:  153; EM: 11; ED:  3; DayOffset: lotDecOne),
    (SY:  153; SM: 12; SD:  3; EY:  154; EM:  1; ED:  1; DayOffset: lotDecOne),
    (SY:  154; SM:  1; SD: 31; EY:  154; EM:  3; ED:  1; DayOffset: lotDecOne),
    (SY:  154; SM:  6; SD: 28; EY:  154; EM:  7; ED: 27; DayOffset: lotDecOne),
    (SY:  154; SM:  8; SD: 26; EY:  154; EM:  9; ED: 24; DayOffset: lotDecOne),
    (SY:  154; SM: 10; SD: 24; EY:  154; EM: 11; ED: 22; DayOffset: lotDecOne),
    (SY:  154; SM: 12; SD: 22; EY:  155; EM:  1; ED: 20; DayOffset: lotDecOne),
    (SY:  155; SM:  2; SD: 19; EY:  155; EM:  3; ED: 20; DayOffset: lotDecOne),
    (SY:  155; SM:  4; SD: 19; EY:  155; EM:  5; ED: 18; DayOffset: lotDecOne),
    (SY:  155; SM:  6; SD: 17; EY:  155; EM:  7; ED: 16; DayOffset: lotDecOne),
    (SY:  155; SM: 11; SD: 12; EY:  155; EM: 12; ED: 11; DayOffset: lotDecOne),
    (SY:  156; SM:  1; SD: 10; EY:  156; EM:  2; ED:  8; DayOffset: lotDecOne),
    (SY:  156; SM:  3; SD:  9; EY:  156; EM:  4; ED:  7; DayOffset: lotDecOne),
    (SY:  156; SM:  5; SD:  7; EY:  156; EM:  6; ED:  5; DayOffset: lotDecOne),
    (SY:  156; SM:  7; SD:  5; EY:  156; EM:  8; ED:  3; DayOffset: lotDecOne),
    (SY:  156; SM:  9; SD:  2; EY:  156; EM: 10; ED:  1; DayOffset: lotDecOne),
    (SY:  157; SM:  3; SD: 28; EY:  157; EM:  4; ED: 26; DayOffset: lotDecOne),
    (SY:  157; SM:  5; SD: 26; EY:  157; EM: 10; ED: 20; DayOffset: lotDecOne),
    (SY:  158; SM:  2; SD: 16; EY:  158; EM:  3; ED: 17; DayOffset: lotIncOne),
    (SY:  158; SM:  6; SD: 14; EY:  158; EM: 11; ED:  8; DayOffset: lotDecOne),
    (SY:  159; SM:  3; SD:  7; EY:  159; EM:  4; ED:  5; DayOffset: lotIncOne),
    (SY:  159; SM:  8; SD:  1; EY:  159; EM:  8; ED: 30; DayOffset: lotDecOne),
    (SY:  159; SM:  9; SD: 29; EY:  159; EM: 11; ED: 27; DayOffset: lotDecOne),
    (SY:  159; SM: 12; SD: 27; EY:  160; EM:  1; ED: 25; DayOffset: lotDecOne),
    (SY:  160; SM:  8; SD: 19; EY:  160; EM:  9; ED: 17; DayOffset: lotDecOne),
    (SY:  160; SM: 10; SD: 17; EY:  160; EM: 11; ED: 15; DayOffset: lotDecOne),
    (SY:  160; SM: 12; SD: 15; EY:  161; EM:  2; ED: 12; DayOffset: lotDecOne),
    (SY:  161; SM:  9; SD:  7; EY:  161; EM: 10; ED:  6; DayOffset: lotDecOne),
    (SY:  161; SM: 11; SD:  5; EY:  161; EM: 12; ED:  4; DayOffset: lotDecOne),
    (SY:  162; SM:  1; SD:  3; EY:  162; EM:  2; ED:  1; DayOffset: lotDecOne),
    (SY:  162; SM:  7; SD: 29; EY:  162; EM:  8; ED: 27; DayOffset: lotDecOne),
    (SY:  162; SM:  9; SD: 26; EY:  162; EM: 10; ED: 25; DayOffset: lotDecOne),
    (SY:  162; SM: 11; SD: 24; EY:  162; EM: 12; ED: 23; DayOffset: lotDecOne),
    (SY:  163; SM:  1; SD: 22; EY:  163; EM:  2; ED: 20; DayOffset: lotDecOne),
    (SY:  163; SM:  3; SD: 22; EY:  163; EM:  4; ED: 20; DayOffset: lotDecOne),
    (SY:  163; SM: 10; SD: 15; EY:  163; EM: 11; ED: 13; DayOffset: lotDecOne),
    (SY:  163; SM: 12; SD: 13; EY:  164; EM:  1; ED: 11; DayOffset: lotDecOne),
    (SY:  164; SM:  2; SD: 10; EY:  164; EM:  3; ED: 10; DayOffset: lotDecOne),
    (SY:  164; SM:  4; SD:  9; EY:  164; EM:  5; ED:  8; DayOffset: lotDecOne),
    (SY:  164; SM:  6; SD:  7; EY:  164; EM:  7; ED:  6; DayOffset: lotDecOne),
    (SY:  164; SM: 12; SD: 31; EY:  165; EM:  1; ED: 29; DayOffset: lotDecOne),
    (SY:  165; SM:  2; SD: 28; EY:  165; EM:  3; ED: 29; DayOffset: lotDecOne),
    (SY:  165; SM:  4; SD: 28; EY:  165; EM:  7; ED: 25; DayOffset: lotDecOne),
    (SY:  165; SM:  8; SD: 24; EY:  165; EM:  9; ED: 22; DayOffset: lotDecOne),
    (SY:  166; SM:  5; SD: 17; EY:  166; EM: 10; ED: 11; DayOffset: lotDecOne),
    (SY:  167; SM:  7; SD:  4; EY:  167; EM: 12; ED: 28; DayOffset: lotDecOne),
    (SY:  168; SM:  7; SD: 22; EY:  168; EM: 10; ED: 18; DayOffset: lotDecOne),
    (SY:  168; SM: 11; SD: 17; EY:  169; EM:  1; ED: 15; DayOffset: lotDecOne),
    (SY:  169; SM:  8; SD: 10; EY:  170; EM:  1; ED:  4; DayOffset: lotDecOne),
    (SY:  170; SM:  7; SD:  1; EY:  170; EM:  7; ED: 30; DayOffset: lotDecOne),
    (SY:  170; SM:  8; SD: 29; EY:  170; EM:  9; ED: 27; DayOffset: lotDecOne),
    (SY:  170; SM: 10; SD: 27; EY:  171; EM:  1; ED: 23; DayOffset: lotDecOne),
    (SY:  171; SM:  2; SD: 22; EY:  171; EM:  3; ED: 23; DayOffset: lotDecOne),
    (SY:  171; SM:  7; SD: 20; EY:  171; EM:  8; ED: 18; DayOffset: lotDecOne),
    (SY:  171; SM:  9; SD: 17; EY:  171; EM: 10; ED: 16; DayOffset: lotDecOne),
    (SY:  171; SM: 11; SD: 15; EY:  171; EM: 12; ED: 14; DayOffset: lotDecOne),
    (SY:  172; SM:  1; SD: 13; EY:  172; EM:  2; ED: 11; DayOffset: lotDecOne),
    (SY:  172; SM:  3; SD: 12; EY:  172; EM:  4; ED: 10; DayOffset: lotDecOne),
    (SY:  172; SM:  5; SD: 10; EY:  172; EM:  6; ED:  8; DayOffset: lotDecOne),
    (SY:  172; SM: 12; SD:  3; EY:  173; EM:  1; ED:  1; DayOffset: lotDecOne),
    (SY:  173; SM:  1; SD: 31; EY:  173; EM:  3; ED:  1; DayOffset: lotDecOne),
    (SY:  173; SM:  3; SD: 31; EY:  173; EM:  4; ED: 29; DayOffset: lotDecOne),
    (SY:  173; SM:  5; SD: 29; EY:  173; EM:  6; ED: 27; DayOffset: lotDecOne),
    (SY:  173; SM:  7; SD: 27; EY:  173; EM:  8; ED: 25; DayOffset: lotDecOne),
    (SY:  174; SM:  2; SD: 19; EY:  174; EM:  3; ED: 20; DayOffset: lotDecOne),
    (SY:  174; SM:  4; SD: 19; EY:  174; EM:  9; ED: 13; DayOffset: lotDecOne),
    (SY:  175; SM:  6; SD:  6; EY:  175; EM: 10; ED:  2; DayOffset: lotDecOne),
    (SY:  175; SM: 11; SD:  1; EY:  175; EM: 11; ED: 30; DayOffset: lotDecOne),
    (SY:  176; SM:  6; SD: 24; EY:  176; EM:  9; ED: 20; DayOffset: lotDecOne),
    (SY:  176; SM: 10; SD: 20; EY:  176; EM: 12; ED: 18; DayOffset: lotDecOne),
    (SY:  177; SM:  7; SD: 13; EY:  177; EM: 12; ED:  7; DayOffset: lotDecOne),
    (SY:  178; SM:  2; SD:  5; EY:  178; EM:  3; ED:  6; DayOffset: lotDecOne),
    (SY:  178; SM:  8; SD:  1; EY:  178; EM: 12; ED: 26; DayOffset: lotDecOne),
    (SY:  179; SM:  1; SD: 25; EY:  179; EM:  2; ED: 23; DayOffset: lotDecOne),
    (SY:  179; SM:  4; SD: 24; EY:  179; EM:  5; ED: 23; DayOffset: lotIncOne),
    (SY:  179; SM:  6; SD: 22; EY:  179; EM:  7; ED: 21; DayOffset: lotDecOne),
    (SY:  179; SM:  8; SD: 20; EY:  179; EM:  9; ED: 18; DayOffset: lotDecOne),
    (SY:  179; SM: 10; SD: 18; EY:  180; EM:  1; ED: 14; DayOffset: lotDecOne),
    (SY:  180; SM:  2; SD: 13; EY:  180; EM:  3; ED: 13; DayOffset: lotDecOne),
    (SY:  180; SM:  9; SD:  7; EY:  180; EM: 10; ED:  6; DayOffset: lotDecOne),
    (SY:  180; SM: 11; SD:  5; EY:  181; EM:  4; ED:  1; DayOffset: lotDecOne),
    (SY:  181; SM:  5; SD:  1; EY:  181; EM:  5; ED: 30; DayOffset: lotDecOne),
    (SY:  182; SM:  1; SD: 22; EY:  182; EM:  6; ED: 18; DayOffset: lotDecOne),
    (SY:  182; SM:  7; SD: 18; EY:  182; EM:  8; ED: 16; DayOffset: lotDecOne),
    (SY:  183; SM:  3; SD: 11; EY:  183; EM:  4; ED:  9; DayOffset: lotDecOne),
    (SY:  183; SM:  5; SD:  9; EY:  183; EM:  9; ED:  4; DayOffset: lotDecOne),
    (SY:  183; SM: 10; SD:  4; EY:  183; EM: 11; ED:  2; DayOffset: lotDecOne),
    (SY:  184; SM:  5; SD: 27; EY:  184; EM:  6; ED: 25; DayOffset: lotDecOne),
    (SY:  184; SM:  7; SD: 25; EY:  184; EM: 11; ED: 20; DayOffset: lotDecOne),
    (SY:  185; SM:  6; SD: 15; EY:  185; EM: 11; ED:  9; DayOffset: lotDecOne),
    (SY:  186; SM:  1; SD:  8; EY:  186; EM:  2; ED:  6; DayOffset: lotDecOne),
    (SY:  186; SM:  7; SD:  4; EY:  186; EM: 11; ED: 28; DayOffset: lotDecOne),
    (SY:  186; SM: 12; SD: 28; EY:  187; EM:  1; ED: 26; DayOffset: lotDecOne),
    (SY:  187; SM:  5; SD: 25; EY:  187; EM:  6; ED: 23; DayOffset: lotDecOne),
    (SY:  187; SM:  7; SD: 23; EY:  187; EM: 12; ED: 17; DayOffset: lotDecOne),
    (SY:  188; SM:  1; SD: 16; EY:  188; EM:  2; ED: 14; DayOffset: lotDecOne),
    (SY:  188; SM:  8; SD: 10; EY:  188; EM:  9; ED:  8; DayOffset: lotDecOne),
    (SY:  188; SM: 10; SD:  8; EY:  189; EM:  3; ED:  4; DayOffset: lotDecOne),
    (SY:  189; SM:  4; SD:  3; EY:  189; EM:  5; ED:  2; DayOffset: lotDecOne),
    (SY:  189; SM: 11; SD: 25; EY:  190; EM:  5; ED: 21; DayOffset: lotDecOne),
    (SY:  190; SM:  6; SD: 20; EY:  190; EM:  7; ED: 19; DayOffset: lotDecOne),
    (SY:  191; SM:  2; SD: 11; EY:  191; EM:  8; ED:  7; DayOffset: lotDecOne),
    (SY:  191; SM:  9; SD:  6; EY:  191; EM: 10; ED:  5; DayOffset: lotDecOne),
    (SY:  192; SM:  3; SD:  1; EY:  192; EM:  3; ED: 30; DayOffset: lotDecOne),
    (SY:  192; SM:  4; SD: 29; EY:  192; EM:  5; ED: 28; DayOffset: lotDecOne),
    (SY:  192; SM:  6; SD: 27; EY:  192; EM: 10; ED: 23; DayOffset: lotDecOne),
    (SY:  193; SM:  5; SD: 18; EY:  193; EM: 11; ED: 11; DayOffset: lotDecOne),
    (SY:  193; SM: 12; SD: 11; EY:  194; EM:  1; ED:  9; DayOffset: lotDecOne),
    (SY:  194; SM:  6; SD:  6; EY:  194; EM: 10; ED: 31; DayOffset: lotDecOne),
    (SY:  194; SM: 11; SD: 30; EY:  194; EM: 12; ED: 29; DayOffset: lotDecOne),
    (SY:  195; SM:  6; SD: 25; EY:  196; EM:  1; ED: 17; DayOffset: lotDecOne),
    (SY:  196; SM:  7; SD: 13; EY:  197; EM:  2; ED:  4; DayOffset: lotDecOne),
    (SY:  197; SM:  8; SD: 30; EY:  198; EM:  2; ED: 23; DayOffset: lotDecOne),
    (SY:  198; SM:  3; SD: 25; EY:  198; EM:  4; ED: 23; DayOffset: lotDecOne),
    (SY:  198; SM: 11; SD: 16; EY:  198; EM: 12; ED: 15; DayOffset: lotDecOne),
    (SY:  199; SM:  1; SD: 14; EY:  199; EM:  5; ED: 12; DayOffset: lotDecOne),
    (SY:  199; SM:  6; SD: 11; EY:  199; EM:  7; ED: 10; DayOffset: lotDecOne),
    (SY:  199; SM:  8; SD:  9; EY:  199; EM:  9; ED:  7; DayOffset: lotDecOne),
    (SY:  200; SM:  2; SD:  2; EY:  200; EM:  3; ED:  2; DayOffset: lotDecOne),
    (SY:  200; SM:  4; SD:  1; EY:  200; EM:  9; ED: 25; DayOffset: lotDecOne),
    (SY:  200; SM: 10; SD: 25; EY:  200; EM: 11; ED: 23; DayOffset: lotDecOne),
    (SY:  201; SM:  2; SD: 20; EY:  201; EM:  3; ED: 21; DayOffset: lotDecOne),
    (SY:  201; SM:  4; SD: 20; EY:  201; EM:  5; ED: 19; DayOffset: lotDecOne),
    (SY:  201; SM:  6; SD: 18; EY:  201; EM:  7; ED: 17; DayOffset: lotDecOne),
    (SY:  201; SM:  8; SD: 16; EY:  201; EM: 10; ED: 14; DayOffset: lotDecOne),
    (SY:  201; SM: 11; SD: 13; EY:  201; EM: 12; ED: 12; DayOffset: lotDecOne),
    (SY:  202; SM:  5; SD:  9; EY:  202; EM: 10; ED:  3; DayOffset: lotDecOne),
    (SY:  202; SM: 11; SD:  2; EY:  202; EM: 12; ED:  1; DayOffset: lotDecOne),
    (SY:  203; SM:  5; SD: 28; EY:  203; EM: 12; ED: 20; DayOffset: lotDecOne),
    (SY:  204; SM:  6; SD: 15; EY:  205; EM:  1; ED:  7; DayOffset: lotDecOne),
    (SY:  205; SM:  8; SD:  2; EY:  206; EM:  1; ED: 26; DayOffset: lotDecOne),
    (SY:  206; SM:  2; SD: 25; EY:  206; EM:  3; ED: 26; DayOffset: lotDecOne),
    (SY:  206; SM:  8; SD: 21; EY:  206; EM:  9; ED: 19; DayOffset: lotDecOne),
    (SY:  206; SM: 10; SD: 19; EY:  207; EM:  4; ED: 14; DayOffset: lotDecOne),
    (SY:  207; SM:  5; SD: 14; EY:  207; EM:  6; ED: 12; DayOffset: lotDecOne),
    (SY:  207; SM:  9; SD:  9; EY:  207; EM: 10; ED:  8; DayOffset: lotDecOne),
    (SY:  207; SM: 11; SD:  7; EY:  207; EM: 12; ED:  6; DayOffset: lotDecOne),
    (SY:  208; SM:  1; SD:  5; EY:  208; EM:  2; ED:  3; DayOffset: lotDecOne),
    (SY:  208; SM:  3; SD:  4; EY:  208; EM:  6; ED: 30; DayOffset: lotDecOne),
    (SY:  208; SM:  7; SD: 30; EY:  208; EM:  8; ED: 28; DayOffset: lotDecOne),
    (SY:  208; SM:  9; SD: 27; EY:  208; EM: 10; ED: 26; DayOffset: lotDecOne),
    (SY:  208; SM: 11; SD: 25; EY:  208; EM: 12; ED: 24; DayOffset: lotDecOne),
    (SY:  209; SM:  1; SD: 23; EY:  209; EM:  2; ED: 21; DayOffset: lotDecOne),
    (SY:  209; SM:  3; SD: 23; EY:  209; EM:  4; ED: 21; DayOffset: lotDecOne),
    (SY:  209; SM:  5; SD: 21; EY:  209; EM:  6; ED: 19; DayOffset: lotDecOne),
    (SY:  209; SM:  7; SD: 19; EY:  209; EM:  9; ED: 16; DayOffset: lotDecOne),
    (SY:  209; SM: 10; SD: 16; EY:  209; EM: 11; ED: 14; DayOffset: lotDecOne),
    (SY:  210; SM:  2; SD: 11; EY:  210; EM:  3; ED: 12; DayOffset: lotDecOne),
    (SY:  210; SM:  4; SD: 11; EY:  210; EM:  5; ED: 10; DayOffset: lotDecOne),
    (SY:  210; SM:  6; SD:  9; EY:  210; EM:  9; ED:  5; DayOffset: lotDecOne),
    (SY:  210; SM: 10; SD:  5; EY:  210; EM: 12; ED:  3; DayOffset: lotDecOne),
    (SY:  211; SM:  3; SD:  2; EY:  211; EM:  3; ED: 31; DayOffset: lotDecOne),
    (SY:  211; SM:  4; SD: 30; EY:  211; EM: 11; ED: 22; DayOffset: lotDecOne),
    (SY:  212; SM:  5; SD: 18; EY:  212; EM: 12; ED: 10; DayOffset: lotDecOne),
    (SY:  213; SM:  6; SD:  6; EY:  213; EM: 12; ED: 29; DayOffset: lotDecOne),
    (SY:  214; SM:  1; SD: 28; EY:  214; EM:  2; ED: 26; DayOffset: lotDecOne),
    (SY:  214; SM:  7; SD: 24; EY:  214; EM:  8; ED: 22; DayOffset: lotDecOne),
    (SY:  214; SM:  9; SD: 21; EY:  215; EM:  3; ED: 17; DayOffset: lotDecOne),
    (SY:  215; SM:  4; SD: 16; EY:  215; EM:  5; ED: 15; DayOffset: lotDecOne),
    (SY:  215; SM:  8; SD: 12; EY:  215; EM:  9; ED: 10; DayOffset: lotDecOne),
    (SY:  215; SM: 10; SD: 10; EY:  215; EM: 11; ED:  8; DayOffset: lotDecOne),
    (SY:  215; SM: 12; SD:  8; EY:  216; EM:  1; ED:  6; DayOffset: lotDecOne),
    (SY:  216; SM:  2; SD:  5; EY:  216; EM:  6; ED:  2; DayOffset: lotDecOne),
    (SY:  216; SM:  7; SD:  2; EY:  216; EM:  7; ED: 31; DayOffset: lotDecOne),
    (SY:  216; SM:  8; SD: 30; EY:  216; EM:  9; ED: 28; DayOffset: lotDecOne),
    (SY:  216; SM: 10; SD: 28; EY:  216; EM: 11; ED: 26; DayOffset: lotDecOne),
    (SY:  216; SM: 12; SD: 26; EY:  217; EM:  1; ED: 24; DayOffset: lotDecOne),
    (SY:  217; SM:  2; SD: 23; EY:  217; EM:  3; ED: 24; DayOffset: lotDecOne),
    (SY:  217; SM:  4; SD: 23; EY:  217; EM:  8; ED: 19; DayOffset: lotDecOne),
    (SY:  217; SM:  9; SD: 18; EY:  217; EM: 10; ED: 17; DayOffset: lotDecOne),
    (SY:  217; SM: 11; SD: 16; EY:  217; EM: 12; ED: 15; DayOffset: lotDecOne),
    (SY:  218; SM:  1; SD: 14; EY:  218; EM:  2; ED: 12; DayOffset: lotDecOne),
    (SY:  218; SM:  3; SD: 14; EY:  218; EM:  4; ED: 12; DayOffset: lotDecOne),
    (SY:  218; SM:  5; SD: 12; EY:  218; EM:  6; ED: 10; DayOffset: lotDecOne),
    (SY:  218; SM:  7; SD: 10; EY:  218; EM:  8; ED:  8; DayOffset: lotDecOne),
    (SY:  218; SM:  9; SD:  7; EY:  218; EM: 11; ED:  5; DayOffset: lotDecOne),
    (SY:  218; SM: 12; SD:  5; EY:  219; EM:  1; ED:  3; DayOffset: lotDecOne),
    (SY:  219; SM:  2; SD:  2; EY:  219; EM:  3; ED:  3; DayOffset: lotDecOne),
    (SY:  219; SM:  4; SD:  2; EY:  219; EM: 10; ED: 25; DayOffset: lotDecOne),
    (SY:  219; SM: 12; SD: 24; EY:  220; EM:  1; ED: 22; DayOffset: lotDecOne),
    (SY:  220; SM:  2; SD: 21; EY:  220; EM:  3; ED: 21; DayOffset: lotDecOne),
    (SY:  220; SM:  4; SD: 20; EY:  220; EM: 11; ED: 12; DayOffset: lotDecOne),
    (SY:  221; SM:  5; SD:  9; EY:  221; EM: 12; ED:  1; DayOffset: lotDecOne),
    (SY:  221; SM: 12; SD: 31; EY:  222; EM:  1; ED: 29; DayOffset: lotDecOne),
    (SY:  222; SM:  6; SD: 26; EY:  222; EM:  7; ED: 25; DayOffset: lotDecOne),
    (SY:  222; SM:  8; SD: 24; EY:  223; EM:  2; ED: 17; DayOffset: lotDecOne),
    (SY:  223; SM:  7; SD: 15; EY:  223; EM:  8; ED: 13; DayOffset: lotDecOne),
    (SY:  223; SM:  9; SD: 12; EY:  223; EM: 10; ED: 11; DayOffset: lotDecOne),
    (SY:  223; SM: 11; SD: 10; EY:  224; EM:  3; ED:  7; DayOffset: lotDecOne),
    (SY:  224; SM:  4; SD:  6; EY:  224; EM:  5; ED:  5; DayOffset: lotDecOne),
    (SY:  224; SM:  6; SD:  4; EY:  224; EM:  7; ED:  3; DayOffset: lotDecOne),
    (SY:  224; SM:  8; SD:  2; EY:  224; EM:  8; ED: 31; DayOffset: lotDecOne),
    (SY:  224; SM:  9; SD: 30; EY:  224; EM: 10; ED: 29; DayOffset: lotDecOne),
    (SY:  224; SM: 11; SD: 28; EY:  224; EM: 12; ED: 27; DayOffset: lotDecOne),
    (SY:  225; SM:  1; SD: 26; EY:  225; EM:  2; ED: 24; DayOffset: lotDecOne),
    (SY:  225; SM:  3; SD: 26; EY:  225; EM:  5; ED: 24; DayOffset: lotDecOne),
    (SY:  225; SM:  6; SD: 23; EY:  225; EM:  7; ED: 22; DayOffset: lotDecOne),
    (SY:  225; SM:  8; SD: 21; EY:  225; EM:  9; ED: 19; DayOffset: lotDecOne),
    (SY:  225; SM: 10; SD: 19; EY:  225; EM: 11; ED: 17; DayOffset: lotDecOne),
    (SY:  225; SM: 12; SD: 17; EY:  226; EM:  1; ED: 15; DayOffset: lotDecOne),
    (SY:  226; SM:  2; SD: 14; EY:  226; EM:  3; ED: 15; DayOffset: lotDecOne),
    (SY:  226; SM:  4; SD: 14; EY:  226; EM:  5; ED: 13; DayOffset: lotDecOne),
    (SY:  226; SM:  6; SD: 12; EY:  226; EM:  7; ED: 11; DayOffset: lotDecOne),
    (SY:  226; SM:  9; SD:  9; EY:  226; EM: 10; ED:  8; DayOffset: lotDecOne),
    (SY:  226; SM: 11; SD:  7; EY:  226; EM: 12; ED:  6; DayOffset: lotDecOne),
    (SY:  227; SM:  1; SD:  5; EY:  227; EM:  2; ED:  3; DayOffset: lotDecOne),
    (SY:  227; SM:  3; SD:  5; EY:  227; EM:  7; ED: 30; DayOffset: lotDecOne),
    (SY:  227; SM:  8; SD: 29; EY:  227; EM:  9; ED: 27; DayOffset: lotDecOne),
    (SY:  227; SM: 11; SD: 26; EY:  227; EM: 12; ED: 25; DayOffset: lotDecOne),
    (SY:  228; SM:  1; SD: 24; EY:  228; EM:  2; ED: 22; DayOffset: lotDecOne),
    (SY:  228; SM:  3; SD: 23; EY:  228; EM: 10; ED: 15; DayOffset: lotDecOne),
    (SY:  229; SM:  4; SD: 11; EY:  229; EM: 11; ED:  3; DayOffset: lotDecOne),
    (SY:  229; SM: 12; SD:  3; EY:  230; EM:  1; ED:  1; DayOffset: lotDecOne),
    (SY:  230; SM:  5; SD: 29; EY:  230; EM:  8; ED: 24; DayOffset: lotDecOne),
    (SY:  230; SM:  8; SD: 25; EY:  230; EM:  9; ED: 24; DayOffset: lotDecTwo),
    (SY:  230; SM:  9; SD: 25; EY:  231; EM:  1; ED: 20; DayOffset: lotDecOne),
    (SY:  231; SM:  6; SD: 17; EY:  231; EM:  7; ED: 16; DayOffset: lotDecOne),
    (SY:  231; SM:  8; SD: 15; EY:  231; EM: 11; ED: 10; DayOffset: lotDecOne),
    (SY:  231; SM: 11; SD: 11; EY:  231; EM: 12; ED: 11; DayOffset: lotDecTwo),
    (SY:  231; SM: 12; SD: 12; EY:  232; EM:  2; ED:  8; DayOffset: lotDecOne),
    (SY:  232; SM:  3; SD:  9; EY:  232; EM:  4; ED:  7; DayOffset: lotDecOne),
    (SY:  232; SM:  7; SD:  5; EY:  232; EM:  8; ED:  3; DayOffset: lotDecOne),
    (SY:  232; SM:  9; SD:  2; EY:  232; EM: 10; ED:  1; DayOffset: lotDecOne),
    (SY:  232; SM: 10; SD: 31; EY:  233; EM:  1; ED: 27; DayOffset: lotDecOne),
    (SY:  233; SM:  2; SD: 26; EY:  233; EM:  4; ED: 26; DayOffset: lotDecOne),
    (SY:  233; SM:  5; SD: 26; EY:  233; EM:  6; ED: 24; DayOffset: lotDecOne),
    (SY:  233; SM:  7; SD: 24; EY:  233; EM:  8; ED: 22; DayOffset: lotDecOne),
    (SY:  233; SM:  9; SD: 21; EY:  233; EM: 10; ED: 20; DayOffset: lotDecOne),
    (SY:  233; SM: 11; SD: 19; EY:  233; EM: 12; ED: 18; DayOffset: lotDecOne),
    (SY:  234; SM:  1; SD: 17; EY:  234; EM:  2; ED: 15; DayOffset: lotDecOne),
    (SY:  234; SM:  3; SD: 17; EY:  234; EM:  4; ED: 15; DayOffset: lotDecOne),
    (SY:  234; SM:  5; SD: 15; EY:  234; EM:  7; ED: 13; DayOffset: lotDecOne),
    (SY:  234; SM:  8; SD: 12; EY:  234; EM:  9; ED: 10; DayOffset: lotDecOne),
    (SY:  234; SM: 10; SD: 10; EY:  234; EM: 11; ED:  8; DayOffset: lotDecOne),
    (SY:  234; SM: 12; SD:  8; EY:  235; EM:  1; ED:  6; DayOffset: lotDecOne),
    (SY:  235; SM:  2; SD:  5; EY:  235; EM:  7; ED:  2; DayOffset: lotDecOne),
    (SY:  235; SM:  8; SD:  1; EY:  235; EM:  8; ED: 30; DayOffset: lotDecOne),
    (SY:  235; SM: 10; SD: 29; EY:  235; EM: 11; ED: 27; DayOffset: lotDecOne),
    (SY:  235; SM: 12; SD: 27; EY:  236; EM:  1; ED: 25; DayOffset: lotDecOne),
    (SY:  236; SM:  2; SD: 24; EY:  236; EM:  9; ED: 17; DayOffset: lotDecOne),
    (SY:  236; SM: 10; SD: 17; EY:  236; EM: 11; ED: 15; DayOffset: lotDecOne),
    (SY:  237; SM:  1; SD: 14; EY:  237; EM:  2; ED: 12; DayOffset: lotDecOne),
    (SY:  237; SM:  3; SD: 14; EY:  237; EM:  4; ED: 11; DayOffset: lotDecOne),
    (SY:  237; SM:  6; SD: 10; EY:  237; EM:  7; ED:  9; DayOffset: lotDecOne),
    (SY:  237; SM:  8; SD:  8; EY:  237; EM:  9; ED:  6; DayOffset: lotDecOne),
    (SY:  238; SM:  1; SD:  3; EY:  238; EM:  2; ED:  1; DayOffset: lotIncOne),
    (SY:  238; SM:  3; SD:  3; EY:  238; EM:  4; ED:  1; DayOffset: lotIncOne),
    (SY:  238; SM:  7; SD: 28; EY:  238; EM:  9; ED: 25; DayOffset: lotDecOne),
    (SY:  238; SM: 10; SD: 25; EY:  238; EM: 11; ED: 23; DayOffset: lotDecOne),
    (SY:  239; SM:  1; SD: 22; EY:  239; EM:  2; ED: 20; DayOffset: lotIncOne),
    (SY:  239; SM:  3; SD: 22; EY:  239; EM:  4; ED: 20; DayOffset: lotIncOne),
    (SY:  239; SM:  8; SD: 16; EY:  239; EM:  9; ED: 14; DayOffset: lotDecOne),
    (SY:  239; SM: 10; SD: 14; EY:  239; EM: 11; ED: 12; DayOffset: lotDecOne),
    (SY:  240; SM:  4; SD:  9; EY:  240; EM:  5; ED:  8; DayOffset: lotIncOne),
    (SY:  240; SM:  9; SD:  3; EY:  240; EM: 10; ED:  2; DayOffset: lotDecOne),
    (SY:  240; SM: 11; SD:  1; EY:  240; EM: 11; ED: 30; DayOffset: lotDecOne),
    (SY:  241; SM:  3; SD: 29; EY:  241; EM:  4; ED: 27; DayOffset: lotIncOne),
    (SY:  241; SM:  9; SD: 22; EY:  241; EM: 10; ED: 21; DayOffset: lotDecOne),
    (SY:  241; SM: 11; SD: 20; EY:  241; EM: 12; ED: 19; DayOffset: lotDecOne),
    (SY:  242; SM:  4; SD: 17; EY:  242; EM:  5; ED: 16; DayOffset: lotIncOne),
    (SY:  242; SM:  6; SD: 15; EY:  242; EM:  7; ED: 14; DayOffset: lotIncOne),
    (SY:  242; SM: 12; SD:  9; EY:  243; EM:  1; ED:  7; DayOffset: lotDecOne),
    (SY:  243; SM:  2; SD:  6; EY:  243; EM:  3; ED:  7; DayOffset: lotDecOne),
    (SY:  243; SM:  9; SD:  1; EY:  243; EM:  9; ED: 30; DayOffset: lotIncOne),
    (SY:  243; SM: 10; SD: 30; EY:  243; EM: 11; ED: 28; DayOffset: lotIncOne),
    (SY:  244; SM:  2; SD: 25; EY:  244; EM:  3; ED: 25; DayOffset: lotDecOne),
    (SY:  244; SM:  4; SD: 24; EY:  244; EM:  5; ED: 23; DayOffset: lotDecOne),
    (SY:  244; SM: 11; SD: 17; EY:  244; EM: 12; ED: 16; DayOffset: lotIncOne),
    (SY:  245; SM:  1; SD: 15; EY:  245; EM:  2; ED: 13; DayOffset: lotIncOne),
    (SY:  245; SM:  5; SD: 13; EY:  245; EM:  6; ED: 11; DayOffset: lotDecOne),
    (SY:  245; SM:  7; SD: 11; EY:  245; EM:  8; ED:  9; DayOffset: lotDecOne),
    (SY:  245; SM: 12; SD:  6; EY:  246; EM:  1; ED:  4; DayOffset: lotIncOne),
    (SY:  246; SM:  2; SD:  3; EY:  246; EM:  3; ED:  4; DayOffset: lotIncOne),
    (SY:  246; SM:  7; SD: 30; EY:  246; EM:  8; ED: 28; DayOffset: lotDecOne),
    (SY:  246; SM:  9; SD: 27; EY:  246; EM: 10; ED: 26; DayOffset: lotDecOne),
    (SY:  246; SM: 12; SD: 25; EY:  247; EM:  1; ED: 23; DayOffset: lotIncOne),
    (SY:  247; SM:  2; SD: 22; EY:  247; EM:  3; ED: 23; DayOffset: lotIncOne),
    (SY:  247; SM:  4; SD: 22; EY:  247; EM:  5; ED: 21; DayOffset: lotIncOne),
    (SY:  247; SM:  7; SD: 19; EY:  247; EM:  8; ED: 17; DayOffset: lotDecOne),
    (SY:  247; SM:  9; SD: 16; EY:  247; EM: 10; ED: 15; DayOffset: lotDecOne),
    (SY:  248; SM:  3; SD: 12; EY:  248; EM:  4; ED: 10; DayOffset: lotIncOne),
    (SY:  248; SM:  5; SD: 10; EY:  248; EM:  6; ED:  8; DayOffset: lotIncOne),
    (SY:  248; SM:  8; SD:  6; EY:  248; EM:  9; ED:  4; DayOffset: lotDecOne),
    (SY:  248; SM: 10; SD:  4; EY:  248; EM: 11; ED:  2; DayOffset: lotDecOne),
    (SY:  249; SM:  3; SD:  1; EY:  249; EM:  4; ED: 29; DayOffset: lotIncOne),
    (SY:  249; SM:  8; SD: 25; EY:  249; EM:  9; ED: 23; DayOffset: lotDecOne),
    (SY:  249; SM: 10; SD: 23; EY:  249; EM: 11; ED: 21; DayOffset: lotDecOne),
    (SY:  249; SM: 12; SD: 21; EY:  250; EM:  1; ED: 19; DayOffset: lotDecOne),
    (SY:  250; SM:  3; SD: 20; EY:  250; EM:  4; ED: 18; DayOffset: lotIncOne),
    (SY:  250; SM:  5; SD: 18; EY:  250; EM:  6; ED: 16; DayOffset: lotIncOne),
    (SY:  250; SM:  7; SD: 16; EY:  250; EM:  8; ED: 14; DayOffset: lotIncOne),
    (SY:  250; SM:  9; SD: 13; EY:  250; EM: 10; ED: 12; DayOffset: lotDecOne),
    (SY:  250; SM: 11; SD: 11; EY:  250; EM: 12; ED: 10; DayOffset: lotDecOne),
    (SY:  251; SM:  1; SD:  9; EY:  251; EM:  2; ED:  7; DayOffset: lotDecOne),
    (SY:  251; SM:  6; SD:  6; EY:  251; EM:  7; ED:  5; DayOffset: lotIncOne),
    (SY:  251; SM:  8; SD:  4; EY:  251; EM:  9; ED:  2; DayOffset: lotIncOne),
    (SY:  251; SM: 10; SD:  2; EY:  251; EM: 10; ED: 31; DayOffset: lotIncOne),
    (SY:  251; SM: 11; SD: 30; EY:  251; EM: 12; ED: 29; DayOffset: lotDecOne),
    (SY:  252; SM:  1; SD: 28; EY:  252; EM:  2; ED: 26; DayOffset: lotDecOne),
    (SY:  252; SM:  3; SD: 27; EY:  252; EM:  4; ED: 25; DayOffset: lotDecOne),
    (SY:  252; SM:  8; SD: 22; EY:  252; EM:  9; ED: 20; DayOffset: lotIncOne),
    (SY:  252; SM: 10; SD: 20; EY:  252; EM: 11; ED: 18; DayOffset: lotIncOne),
    (SY:  252; SM: 12; SD: 18; EY:  253; EM:  1; ED: 16; DayOffset: lotIncOne),
    (SY:  253; SM:  4; SD: 15; EY:  253; EM:  5; ED: 14; DayOffset: lotDecOne),
    (SY:  253; SM:  6; SD: 13; EY:  253; EM:  7; ED: 12; DayOffset: lotDecOne),
    (SY:  253; SM: 11; SD:  8; EY:  253; EM: 12; ED:  7; DayOffset: lotIncOne),
    (SY:  254; SM:  1; SD:  6; EY:  254; EM:  2; ED:  4; DayOffset: lotIncOne),
    (SY:  254; SM:  8; SD: 30; EY:  254; EM:  9; ED: 28; DayOffset: lotDecOne),
    (SY:  254; SM: 11; SD: 27; EY:  254; EM: 12; ED: 26; DayOffset: lotIncOne),
    (SY:  255; SM:  1; SD: 25; EY:  255; EM:  2; ED: 23; DayOffset: lotIncOne),
    (SY:  255; SM:  6; SD: 21; EY:  255; EM:  7; ED: 20; DayOffset: lotDecOne),
    (SY:  255; SM:  8; SD: 19; EY:  255; EM:  9; ED: 17; DayOffset: lotDecOne),
    (SY:  256; SM:  2; SD: 13; EY:  256; EM:  3; ED: 13; DayOffset: lotIncOne),
    (SY:  256; SM:  4; SD: 12; EY:  256; EM:  5; ED: 11; DayOffset: lotIncOne),
    (SY:  256; SM:  7; SD:  9; EY:  256; EM:  8; ED:  7; DayOffset: lotDecOne),
    (SY:  256; SM:  9; SD:  6; EY:  256; EM: 10; ED:  5; DayOffset: lotDecOne),
    (SY:  257; SM:  2; SD:  1; EY:  257; EM:  4; ED:  1; DayOffset: lotIncOne),
    (SY:  257; SM:  7; SD: 28; EY:  257; EM:  8; ED: 26; DayOffset: lotDecOne),
    (SY:  257; SM:  9; SD: 25; EY:  257; EM: 10; ED: 24; DayOffset: lotDecOne),
    (SY:  257; SM: 11; SD: 23; EY:  257; EM: 12; ED: 22; DayOffset: lotDecOne),
    (SY:  258; SM:  2; SD: 20; EY:  258; EM:  3; ED: 21; DayOffset: lotIncOne),
    (SY:  258; SM:  4; SD: 20; EY:  258; EM:  5; ED: 19; DayOffset: lotIncOne),
    (SY:  258; SM:  6; SD: 18; EY:  258; EM:  7; ED: 17; DayOffset: lotIncOne),
    (SY:  258; SM:  8; SD: 16; EY:  258; EM:  9; ED: 14; DayOffset: lotDecOne),
    (SY:  258; SM: 10; SD: 14; EY:  258; EM: 11; ED: 12; DayOffset: lotDecOne),
    (SY:  258; SM: 12; SD: 12; EY:  259; EM:  1; ED: 10; DayOffset: lotDecOne),
    (SY:  259; SM:  5; SD:  9; EY:  259; EM:  6; ED:  7; DayOffset: lotIncOne),
    (SY:  259; SM:  7; SD:  7; EY:  259; EM:  8; ED:  5; DayOffset: lotIncOne),
    (SY:  259; SM: 11; SD:  2; EY:  259; EM: 12; ED:  1; DayOffset: lotDecOne),
    (SY:  259; SM: 12; SD: 31; EY:  260; EM:  1; ED: 29; DayOffset: lotDecOne),
    (SY:  260; SM:  7; SD: 25; EY:  260; EM:  8; ED: 23; DayOffset: lotIncOne),
    (SY:  261; SM:  3; SD: 18; EY:  261; EM:  4; ED: 16; DayOffset: lotDecOne),
    (SY:  261; SM:  5; SD: 16; EY:  261; EM:  6; ED: 14; DayOffset: lotDecOne),
    (SY:  262; SM:  8; SD:  2; EY:  262; EM:  8; ED: 31; DayOffset: lotDecOne),
    (SY:  262; SM: 12; SD: 28; EY:  263; EM:  1; ED: 26; DayOffset: lotIncOne),
    (SY:  263; SM:  7; SD: 22; EY:  263; EM:  8; ED: 20; DayOffset: lotDecOne),
    (SY:  264; SM:  1; SD: 16; EY:  264; EM:  2; ED: 14; DayOffset: lotIncOne),
    (SY:  264; SM:  6; SD: 11; EY:  264; EM:  7; ED: 10; DayOffset: lotDecOne),
    (SY:  264; SM:  8; SD:  9; EY:  264; EM:  9; ED:  7; DayOffset: lotDecOne),
    (SY:  265; SM:  1; SD:  4; EY:  265; EM:  3; ED:  4; DayOffset: lotIncOne),
    (SY:  265; SM:  6; SD: 30; EY:  265; EM:  7; ED: 29; DayOffset: lotDecOne),
    (SY:  265; SM:  8; SD: 28; EY:  265; EM:  9; ED: 26; DayOffset: lotDecOne),
    (SY:  265; SM: 10; SD: 26; EY:  265; EM: 11; ED: 24; DayOffset: lotDecOne),
    (SY:  266; SM:  1; SD: 23; EY:  266; EM:  2; ED: 21; DayOffset: lotIncOne),
    (SY:  266; SM:  3; SD: 23; EY:  266; EM:  4; ED: 21; DayOffset: lotIncOne),
    (SY:  266; SM:  5; SD: 21; EY:  266; EM:  6; ED: 19; DayOffset: lotIncOne),
    (SY:  266; SM:  7; SD: 19; EY:  266; EM:  8; ED: 17; DayOffset: lotDecOne),
    (SY:  266; SM:  9; SD: 16; EY:  266; EM: 10; ED: 15; DayOffset: lotDecOne),
    (SY:  266; SM: 11; SD: 14; EY:  266; EM: 12; ED: 13; DayOffset: lotDecOne),
    (SY:  267; SM:  2; SD: 11; EY:  267; EM:  3; ED: 12; DayOffset: lotIncOne),
    (SY:  267; SM:  4; SD: 11; EY:  267; EM:  5; ED: 10; DayOffset: lotIncOne),
    (SY:  267; SM:  6; SD:  9; EY:  267; EM:  7; ED:  8; DayOffset: lotIncOne),
    (SY:  267; SM: 10; SD:  5; EY:  267; EM: 11; ED:  3; DayOffset: lotDecOne),
    (SY:  267; SM: 12; SD:  3; EY:  268; EM:  1; ED:  1; DayOffset: lotDecOne),
    (SY:  268; SM:  4; SD: 29; EY:  268; EM:  5; ED: 28; DayOffset: lotIncOne),
    (SY:  268; SM:  6; SD: 27; EY:  268; EM:  7; ED: 26; DayOffset: lotIncOne),
    (SY:  269; SM:  2; SD: 18; EY:  269; EM:  3; ED: 19; DayOffset: lotDecOne),
    (SY:  271; SM: 12; SD: 19; EY:  272; EM:  1; ED: 17; DayOffset: lotIncOne),
    (SY:  272; SM:  5; SD: 14; EY:  272; EM:  6; ED: 12; DayOffset: lotDecOne),
    (SY:  272; SM:  7; SD: 12; EY:  272; EM:  8; ED: 10; DayOffset: lotDecOne),
    (SY:  272; SM: 12; SD:  7; EY:  273; EM:  2; ED:  4; DayOffset: lotIncOne),
    (SY:  273; SM:  6; SD:  2; EY:  273; EM:  7; ED:  1; DayOffset: lotDecOne),
    (SY:  273; SM:  7; SD: 31; EY:  273; EM:  8; ED: 29; DayOffset: lotDecOne),
    (SY:  273; SM:  9; SD: 28; EY:  273; EM: 10; ED: 27; DayOffset: lotDecOne),
    (SY:  273; SM: 12; SD: 26; EY:  274; EM:  1; ED: 24; DayOffset: lotIncOne),
    (SY:  274; SM:  2; SD: 23; EY:  274; EM:  3; ED: 24; DayOffset: lotIncOne),
    (SY:  274; SM:  4; SD: 23; EY:  274; EM:  5; ED: 22; DayOffset: lotIncOne),
    (SY:  274; SM:  6; SD: 21; EY:  274; EM:  7; ED: 20; DayOffset: lotDecOne),
    (SY:  274; SM:  8; SD: 19; EY:  274; EM:  9; ED: 17; DayOffset: lotDecOne),
    (SY:  274; SM: 10; SD: 17; EY:  274; EM: 11; ED: 15; DayOffset: lotDecOne),
    (SY:  275; SM:  1; SD: 14; EY:  275; EM:  2; ED: 12; DayOffset: lotIncOne),
    (SY:  275; SM:  3; SD: 14; EY:  275; EM:  4; ED: 12; DayOffset: lotIncOne),
    (SY:  275; SM:  5; SD: 12; EY:  275; EM:  6; ED: 10; DayOffset: lotIncOne),
    (SY:  275; SM:  9; SD:  7; EY:  275; EM: 10; ED:  6; DayOffset: lotDecOne),
    (SY:  275; SM: 11; SD:  5; EY:  275; EM: 12; ED:  4; DayOffset: lotDecOne),
    (SY:  276; SM:  4; SD:  1; EY:  276; EM:  4; ED: 30; DayOffset: lotIncOne),
    (SY:  276; SM:  5; SD: 30; EY:  276; EM:  6; ED: 28; DayOffset: lotIncOne),
    (SY:  277; SM:  1; SD: 21; EY:  277; EM:  2; ED: 19; DayOffset: lotDecOne),
    (SY:  277; SM:  6; SD: 18; EY:  277; EM:  7; ED: 17; DayOffset: lotIncOne),
    (SY:  280; SM:  4; SD: 16; EY:  280; EM:  5; ED: 15; DayOffset: lotDecOne),
    (SY:  280; SM:  6; SD: 14; EY:  280; EM:  7; ED: 13; DayOffset: lotDecOne),
    (SY:  280; SM: 11; SD:  9; EY:  280; EM: 12; ED:  8; DayOffset: lotIncOne),
    (SY:  281; SM:  5; SD:  5; EY:  281; EM:  6; ED:  3; DayOffset: lotDecOne),
    (SY:  281; SM:  7; SD:  3; EY:  281; EM:  8; ED:  1; DayOffset: lotDecOne),
    (SY:  281; SM:  8; SD: 31; EY:  281; EM:  9; ED: 29; DayOffset: lotDecOne),
    (SY:  281; SM: 11; SD: 28; EY:  281; EM: 12; ED: 27; DayOffset: lotIncOne),
    (SY:  282; SM:  1; SD: 26; EY:  282; EM:  2; ED: 24; DayOffset: lotIncOne),
    (SY:  282; SM:  3; SD: 26; EY:  282; EM:  4; ED: 24; DayOffset: lotIncOne),
    (SY:  282; SM:  5; SD: 24; EY:  282; EM:  6; ED: 22; DayOffset: lotDecOne),
    (SY:  282; SM:  7; SD: 22; EY:  282; EM:  8; ED: 20; DayOffset: lotDecOne),
    (SY:  282; SM:  9; SD: 19; EY:  282; EM: 10; ED: 18; DayOffset: lotDecOne),
    (SY:  282; SM: 12; SD: 17; EY:  283; EM:  1; ED: 15; DayOffset: lotIncOne),
    (SY:  283; SM:  2; SD: 14; EY:  283; EM:  3; ED: 15; DayOffset: lotIncOne),
    (SY:  283; SM:  4; SD: 14; EY:  283; EM:  5; ED: 13; DayOffset: lotIncOne),
    (SY:  283; SM:  8; SD: 10; EY:  283; EM: 11; ED:  6; DayOffset: lotDecOne),
    (SY:  284; SM:  3; SD:  4; EY:  284; EM:  4; ED:  2; DayOffset: lotIncOne),
    (SY:  284; SM:  5; SD:  2; EY:  284; EM:  5; ED: 31; DayOffset: lotIncOne),
    (SY:  284; SM:  9; SD: 26; EY:  284; EM: 10; ED: 25; DayOffset: lotDecOne),
    (SY:  284; SM: 12; SD: 24; EY:  285; EM:  1; ED: 22; DayOffset: lotDecOne),
    (SY:  285; SM:  3; SD: 23; EY:  285; EM:  4; ED: 21; DayOffset: lotIncOne),
    (SY:  285; SM:  5; SD: 21; EY:  285; EM:  6; ED: 19; DayOffset: lotIncOne),
    (SY:  286; SM:  3; SD: 12; EY:  286; EM:  4; ED: 10; DayOffset: lotIncOne),
    (SY:  287; SM:  5; SD: 29; EY:  287; EM:  6; ED: 27; DayOffset: lotIncOne),
    (SY:  288; SM:  8; SD: 14; EY:  288; EM:  9; ED: 12; DayOffset: lotIncOne),
    (SY:  288; SM: 10; SD: 12; EY:  288; EM: 11; ED: 10; DayOffset: lotIncOne),
    (SY:  289; SM:  4; SD:  7; EY:  289; EM:  5; ED:  6; DayOffset: lotDecOne),
    (SY:  289; SM:  6; SD:  5; EY:  289; EM:  7; ED:  4; DayOffset: lotDecOne),
    (SY:  289; SM: 10; SD: 31; EY:  289; EM: 11; ED: 29; DayOffset: lotIncOne),
    (SY:  289; SM: 12; SD: 29; EY:  290; EM:  1; ED: 27; DayOffset: lotIncOne),
    (SY:  290; SM:  2; SD: 26; EY:  290; EM:  3; ED: 27; DayOffset: lotIncOne),
    (SY:  290; SM:  4; SD: 26; EY:  290; EM:  5; ED: 25; DayOffset: lotDecOne),
    (SY:  290; SM:  6; SD: 24; EY:  290; EM:  7; ED: 23; DayOffset: lotDecOne),
    (SY:  290; SM:  8; SD: 22; EY:  290; EM:  9; ED: 20; DayOffset: lotDecOne),
    (SY:  291; SM:  1; SD: 17; EY:  291; EM:  2; ED: 15; DayOffset: lotIncOne),
    (SY:  291; SM:  3; SD: 17; EY:  291; EM:  4; ED: 15; DayOffset: lotIncOne),
    (SY:  291; SM:  7; SD: 13; EY:  291; EM: 10; ED:  9; DayOffset: lotDecOne),
    (SY:  291; SM: 11; SD:  8; EY:  291; EM: 12; ED:  7; DayOffset: lotDecOne),
    (SY:  292; SM:  2; SD:  5; EY:  292; EM:  3; ED:  5; DayOffset: lotIncOne),
    (SY:  292; SM:  4; SD:  4; EY:  292; EM:  5; ED:  3; DayOffset: lotIncOne),
    (SY:  292; SM:  8; SD: 29; EY:  292; EM:  9; ED: 27; DayOffset: lotDecOne),
    (SY:  292; SM: 11; SD: 26; EY:  292; EM: 12; ED: 25; DayOffset: lotDecOne),
    (SY:  293; SM:  2; SD: 23; EY:  293; EM:  3; ED: 24; DayOffset: lotIncOne),
    (SY:  293; SM:  4; SD: 23; EY:  293; EM:  5; ED: 22; DayOffset: lotIncOne),
    (SY:  293; SM:  9; SD: 17; EY:  293; EM: 10; ED: 16; DayOffset: lotDecOne),
    (SY:  293; SM: 11; SD: 15; EY:  293; EM: 12; ED: 14; DayOffset: lotDecOne),
    (SY:  294; SM:  2; SD: 12; EY:  294; EM:  3; ED: 13; DayOffset: lotIncOne),
    (SY:  294; SM:  5; SD: 12; EY:  294; EM:  6; ED: 10; DayOffset: lotIncOne),
    (SY:  294; SM: 10; SD:  6; EY:  294; EM: 11; ED:  4; DayOffset: lotDecOne),
    (SY:  294; SM: 12; SD:  4; EY:  295; EM:  1; ED:  2; DayOffset: lotDecOne),
    (SY:  295; SM:  5; SD:  1; EY:  295; EM:  5; ED: 30; DayOffset: lotIncOne),
    (SY:  295; SM: 12; SD: 23; EY:  296; EM:  1; ED: 21; DayOffset: lotDecOne),
    (SY:  296; SM:  7; SD: 17; EY:  296; EM:  8; ED: 15; DayOffset: lotIncOne),
    (SY:  296; SM:  9; SD: 14; EY:  296; EM: 10; ED: 13; DayOffset: lotIncOne),
    (SY:  297; SM:  3; SD: 10; EY:  297; EM:  4; ED:  8; DayOffset: lotDecOne),
    (SY:  297; SM:  5; SD:  8; EY:  297; EM:  6; ED:  6; DayOffset: lotDecOne),
    (SY:  297; SM: 10; SD:  3; EY:  297; EM: 11; ED:  1; DayOffset: lotIncOne),
    (SY:  297; SM: 12; SD:  1; EY:  297; EM: 12; ED: 30; DayOffset: lotIncOne),
    (SY:  298; SM:  1; SD: 29; EY:  298; EM:  2; ED: 27; DayOffset: lotIncOne),
    (SY:  298; SM:  3; SD: 29; EY:  298; EM:  4; ED: 27; DayOffset: lotDecOne),
    (SY:  298; SM:  5; SD: 27; EY:  298; EM:  6; ED: 25; DayOffset: lotDecOne),
    (SY:  298; SM:  7; SD: 25; EY:  298; EM:  8; ED: 23; DayOffset: lotDecOne),
    (SY:  298; SM: 12; SD: 20; EY:  299; EM:  1; ED: 18; DayOffset: lotIncOne),
    (SY:  299; SM:  2; SD: 17; EY:  299; EM:  3; ED: 18; DayOffset: lotIncOne),
    (SY:  299; SM:  6; SD: 15; EY:  299; EM:  7; ED: 14; DayOffset: lotDecOne),
    (SY:  299; SM:  8; SD: 13; EY:  299; EM:  9; ED: 11; DayOffset: lotDecOne),
    (SY:  299; SM: 10; SD: 11; EY:  299; EM: 11; ED:  9; DayOffset: lotDecOne),
    (SY:  300; SM:  1; SD:  8; EY:  300; EM:  2; ED:  6; DayOffset: lotIncOne),
    (SY:  300; SM:  3; SD:  7; EY:  300; EM:  4; ED:  5; DayOffset: lotIncOne),
    (SY:  300; SM:  8; SD:  1; EY:  300; EM:  8; ED: 30; DayOffset: lotDecOne),
    (SY:  300; SM:  9; SD: 29; EY:  300; EM: 11; ED: 27; DayOffset: lotDecOne),
    (SY:  301; SM:  1; SD: 26; EY:  301; EM:  2; ED: 24; DayOffset: lotIncOne),
    (SY:  301; SM:  3; SD: 26; EY:  301; EM:  4; ED: 24; DayOffset: lotIncOne),
    (SY:  301; SM:  8; SD: 20; EY:  301; EM:  9; ED: 18; DayOffset: lotDecOne),
    (SY:  301; SM: 10; SD: 18; EY:  301; EM: 11; ED: 16; DayOffset: lotDecOne),
    (SY:  302; SM:  1; SD: 15; EY:  302; EM:  2; ED: 13; DayOffset: lotIncOne),
    (SY:  302; SM:  4; SD: 14; EY:  302; EM:  5; ED: 13; DayOffset: lotIncOne),
    (SY:  302; SM:  9; SD:  8; EY:  302; EM: 10; ED:  7; DayOffset: lotDecOne),
    (SY:  302; SM: 11; SD:  6; EY:  302; EM: 12; ED:  5; DayOffset: lotDecOne),
    (SY:  303; SM:  4; SD:  3; EY:  303; EM:  5; ED:  2; DayOffset: lotIncOne),
    (SY:  303; SM:  9; SD: 27; EY:  303; EM: 10; ED: 26; DayOffset: lotDecOne),
    (SY:  303; SM: 11; SD: 25; EY:  303; EM: 12; ED: 24; DayOffset: lotDecOne),
    (SY:  304; SM:  1; SD: 23; EY:  304; EM:  2; ED: 21; DayOffset: lotDecOne),
    (SY:  304; SM:  4; SD: 21; EY:  304; EM:  5; ED: 20; DayOffset: lotIncOne),
    (SY:  304; SM:  6; SD: 19; EY:  304; EM:  7; ED: 18; DayOffset: lotIncOne),
    (SY:  304; SM:  8; SD: 17; EY:  304; EM:  9; ED: 15; DayOffset: lotIncOne),
    (SY:  304; SM: 12; SD: 13; EY:  305; EM:  1; ED: 11; DayOffset: lotDecOne),
    (SY:  305; SM:  2; SD: 10; EY:  305; EM:  3; ED: 11; DayOffset: lotDecOne),
    (SY:  305; SM:  9; SD:  5; EY:  305; EM: 10; ED:  4; DayOffset: lotIncOne),
    (SY:  305; SM: 11; SD:  3; EY:  305; EM: 12; ED:  2; DayOffset: lotIncOne),
    (SY:  306; SM:  1; SD:  1; EY:  306; EM:  1; ED: 30; DayOffset: lotIncOne),
    (SY:  306; SM:  3; SD:  1; EY:  306; EM:  3; ED: 30; DayOffset: lotDecOne),
    (SY:  306; SM:  4; SD: 29; EY:  306; EM:  5; ED: 28; DayOffset: lotDecOne),
    (SY:  306; SM:  6; SD: 27; EY:  306; EM:  7; ED: 26; DayOffset: lotDecOne),
    (SY:  306; SM: 11; SD: 22; EY:  306; EM: 12; ED: 21; DayOffset: lotIncOne),
    (SY:  307; SM:  1; SD: 20; EY:  307; EM:  2; ED: 18; DayOffset: lotIncOne),
    (SY:  307; SM:  5; SD: 18; EY:  307; EM:  6; ED: 16; DayOffset: lotDecOne),
    (SY:  307; SM:  7; SD: 16; EY:  307; EM:  8; ED: 14; DayOffset: lotDecOne),
    (SY:  307; SM: 12; SD: 11; EY:  308; EM:  1; ED:  9; DayOffset: lotIncOne),
    (SY:  308; SM:  2; SD:  8; EY:  308; EM:  3; ED:  8; DayOffset: lotIncOne),
    (SY:  308; SM:  7; SD:  4; EY:  308; EM:  8; ED:  2; DayOffset: lotDecOne),
    (SY:  308; SM: 10; SD:  1; EY:  308; EM: 10; ED: 30; DayOffset: lotDecOne),
    (SY:  308; SM: 12; SD: 29; EY:  309; EM:  1; ED: 27; DayOffset: lotIncOne),
    (SY:  309; SM:  2; SD: 26; EY:  309; EM:  3; ED: 27; DayOffset: lotIncOne),
    (SY:  309; SM:  7; SD: 23; EY:  309; EM:  8; ED: 21; DayOffset: lotDecOne),
    (SY:  309; SM:  9; SD: 20; EY:  309; EM: 10; ED: 19; DayOffset: lotDecOne),
    (SY:  309; SM: 12; SD: 18; EY:  310; EM:  1; ED: 16; DayOffset: lotIncOne),
    (SY:  310; SM:  3; SD: 17; EY:  310; EM:  4; ED: 15; DayOffset: lotIncOne),
    (SY:  310; SM:  8; SD: 11; EY:  310; EM:  9; ED:  9; DayOffset: lotDecOne),
    (SY:  310; SM: 10; SD:  9; EY:  310; EM: 11; ED:  7; DayOffset: lotDecOne),
    (SY:  311; SM:  3; SD:  6; EY:  311; EM:  5; ED:  4; DayOffset: lotIncOne),
    (SY:  311; SM:  8; SD: 30; EY:  311; EM:  9; ED: 28; DayOffset: lotDecOne),
    (SY:  311; SM: 10; SD: 28; EY:  311; EM: 11; ED: 26; DayOffset: lotDecOne),
    (SY:  311; SM: 12; SD: 26; EY:  312; EM:  1; ED: 24; DayOffset: lotDecOne),
    (SY:  312; SM:  3; SD: 24; EY:  312; EM:  4; ED: 22; DayOffset: lotIncOne),
    (SY:  312; SM:  5; SD: 22; EY:  312; EM:  6; ED: 20; DayOffset: lotIncOne),
    (SY:  312; SM:  7; SD: 20; EY:  312; EM:  8; ED: 18; DayOffset: lotIncOne),
    (SY:  312; SM:  9; SD: 17; EY:  312; EM: 10; ED: 16; DayOffset: lotDecOne),
    (SY:  312; SM: 11; SD: 15; EY:  312; EM: 12; ED: 14; DayOffset: lotDecOne),
    (SY:  313; SM:  1; SD: 13; EY:  313; EM:  2; ED: 11; DayOffset: lotDecOne),
    (SY:  313; SM:  6; SD: 10; EY:  313; EM:  7; ED:  9; DayOffset: lotIncOne),
    (SY:  313; SM:  8; SD:  8; EY:  313; EM:  9; ED:  6; DayOffset: lotIncOne),
    (SY:  313; SM: 10; SD:  6; EY:  313; EM: 11; ED:  4; DayOffset: lotIncOne),
    (SY:  314; SM:  2; SD:  1; EY:  314; EM:  3; ED:  2; DayOffset: lotDecOne),
    (SY:  314; SM:  4; SD:  1; EY:  314; EM:  4; ED: 30; DayOffset: lotDecOne),
    (SY:  314; SM: 10; SD: 25; EY:  314; EM: 11; ED: 23; DayOffset: lotIncOne),
    (SY:  315; SM:  4; SD: 20; EY:  315; EM:  5; ED: 19; DayOffset: lotDecOne),
    (SY:  315; SM:  6; SD: 18; EY:  315; EM:  7; ED: 17; DayOffset: lotDecOne),
    (SY:  315; SM: 11; SD: 13; EY:  315; EM: 12; ED: 12; DayOffset: lotIncOne),
    (SY:  316; SM:  1; SD: 11; EY:  316; EM:  2; ED:  9; DayOffset: lotIncOne),
    (SY:  316; SM:  9; SD:  3; EY:  316; EM: 10; ED:  2; DayOffset: lotDecOne),
    (SY:  316; SM: 12; SD:  1; EY:  316; EM: 12; ED: 30; DayOffset: lotIncOne),
    (SY:  317; SM:  1; SD: 29; EY:  317; EM:  2; ED: 27; DayOffset: lotIncOne),
    (SY:  317; SM:  6; SD: 25; EY:  317; EM:  7; ED: 24; DayOffset: lotDecOne),
    (SY:  317; SM:  8; SD: 23; EY:  317; EM:  9; ED: 21; DayOffset: lotDecOne),
    (SY:  317; SM: 11; SD: 20; EY:  317; EM: 12; ED: 19; DayOffset: lotIncOne),
    (SY:  318; SM:  2; SD: 17; EY:  318; EM:  3; ED: 18; DayOffset: lotIncOne),
    (SY:  318; SM:  7; SD: 14; EY:  318; EM:  8; ED: 12; DayOffset: lotDecOne),
    (SY:  318; SM:  9; SD: 11; EY:  318; EM: 10; ED: 10; DayOffset: lotDecOne),
    (SY:  319; SM:  2; SD:  6; EY:  319; EM:  4; ED:  6; DayOffset: lotIncOne),
    (SY:  319; SM:  8; SD:  2; EY:  319; EM:  8; ED: 31; DayOffset: lotDecOne),
    (SY:  319; SM:  9; SD: 30; EY:  319; EM: 10; ED: 29; DayOffset: lotDecOne),
    (SY:  319; SM: 11; SD: 28; EY:  319; EM: 12; ED: 27; DayOffset: lotDecOne),
    (SY:  320; SM:  2; SD: 25; EY:  320; EM:  3; ED: 25; DayOffset: lotIncOne),
    (SY:  320; SM:  4; SD: 24; EY:  320; EM:  5; ED: 23; DayOffset: lotIncOne),
    (SY:  320; SM:  6; SD: 22; EY:  320; EM:  7; ED: 21; DayOffset: lotIncOne),
    (SY:  320; SM:  8; SD: 20; EY:  320; EM:  9; ED: 18; DayOffset: lotDecOne),
    (SY:  320; SM: 10; SD: 18; EY:  320; EM: 11; ED: 16; DayOffset: lotDecOne),
    (SY:  320; SM: 12; SD: 16; EY:  321; EM:  1; ED: 14; DayOffset: lotDecOne),
    (SY:  321; SM:  5; SD: 13; EY:  321; EM:  6; ED: 11; DayOffset: lotIncOne),
    (SY:  321; SM:  7; SD: 11; EY:  321; EM:  8; ED:  9; DayOffset: lotIncOne),
    (SY:  322; SM:  1; SD:  4; EY:  322; EM:  2; ED:  2; DayOffset: lotDecOne),
    (SY:  322; SM:  3; SD:  4; EY:  322; EM:  4; ED:  2; DayOffset: lotDecOne),
    (SY:  322; SM:  7; SD: 30; EY:  322; EM:  8; ED: 28; DayOffset: lotIncOne),
    (SY:  323; SM:  3; SD: 23; EY:  323; EM:  4; ED: 21; DayOffset: lotDecOne),
    (SY:  323; SM:  5; SD: 21; EY:  323; EM:  6; ED: 19; DayOffset: lotDecOne),
    (SY:  324; SM:  8; SD:  6; EY:  324; EM:  9; ED:  4; DayOffset: lotDecOne),
    (SY:  325; SM:  1; SD:  1; EY:  325; EM:  1; ED: 30; DayOffset: lotIncOne),
    (SY:  325; SM:  5; SD: 28; EY:  325; EM:  6; ED: 26; DayOffset: lotDecOne),
    (SY:  325; SM:  7; SD: 26; EY:  325; EM:  8; ED: 24; DayOffset: lotDecOne),
    (SY:  325; SM: 10; SD: 23; EY:  325; EM: 11; ED: 21; DayOffset: lotIncOne),
    (SY:  326; SM:  1; SD: 20; EY:  326; EM:  2; ED: 18; DayOffset: lotIncOne),
    (SY:  326; SM:  6; SD: 16; EY:  326; EM:  7; ED: 15; DayOffset: lotDecOne),
    (SY:  326; SM:  8; SD: 14; EY:  326; EM:  9; ED: 12; DayOffset: lotDecOne),
    (SY:  327; SM:  1; SD:  9; EY:  327; EM:  3; ED:  9; DayOffset: lotIncOne),
    (SY:  327; SM:  7; SD:  5; EY:  327; EM:  8; ED:  3; DayOffset: lotDecOne),
    (SY:  327; SM:  9; SD:  2; EY:  327; EM: 10; ED:  1; DayOffset: lotDecOne),
    (SY:  327; SM: 10; SD: 31; EY:  327; EM: 11; ED: 29; DayOffset: lotDecOne),
    (SY:  328; SM:  1; SD: 28; EY:  328; EM:  2; ED: 26; DayOffset: lotIncOne),
    (SY:  328; SM:  3; SD: 27; EY:  328; EM:  4; ED: 25; DayOffset: lotIncOne),
    (SY:  328; SM:  5; SD: 25; EY:  328; EM:  6; ED: 23; DayOffset: lotIncOne),
    (SY:  328; SM:  7; SD: 23; EY:  328; EM:  8; ED: 21; DayOffset: lotDecOne),
    (SY:  328; SM:  9; SD: 20; EY:  328; EM: 10; ED: 19; DayOffset: lotDecOne),
    (SY:  328; SM: 11; SD: 18; EY:  328; EM: 12; ED: 17; DayOffset: lotDecOne),
    (SY:  329; SM:  2; SD: 15; EY:  329; EM:  3; ED: 16; DayOffset: lotIncOne),
    (SY:  329; SM:  4; SD: 15; EY:  329; EM:  5; ED: 14; DayOffset: lotIncOne),
    (SY:  329; SM:  6; SD: 13; EY:  329; EM:  7; ED: 12; DayOffset: lotIncOne),
    (SY:  329; SM: 12; SD:  7; EY:  330; EM:  1; ED:  5; DayOffset: lotDecOne),
    (SY:  330; SM:  5; SD:  4; EY:  330; EM:  6; ED:  2; DayOffset: lotIncOne),
    (SY:  330; SM:  7; SD:  2; EY:  330; EM:  7; ED: 31; DayOffset: lotIncOne),
    (SY:  331; SM:  2; SD: 23; EY:  331; EM:  3; ED: 24; DayOffset: lotDecOne),
    (SY:  333; SM:  9; SD: 25; EY:  333; EM: 10; ED: 24; DayOffset: lotIncOne),
    (SY:  334; SM:  5; SD: 19; EY:  334; EM:  6; ED: 17; DayOffset: lotDecOne),
    (SY:  334; SM:  7; SD: 17; EY:  334; EM:  8; ED: 15; DayOffset: lotDecOne),
    (SY:  334; SM: 12; SD: 12; EY:  335; EM:  1; ED: 10; DayOffset: lotIncOne),
    (SY:  335; SM:  6; SD:  7; EY:  335; EM:  7; ED:  6; DayOffset: lotDecOne),
    (SY:  335; SM:  8; SD:  5; EY:  335; EM:  9; ED:  3; DayOffset: lotDecOne),
    (SY:  335; SM: 10; SD:  3; EY:  335; EM: 11; ED:  1; DayOffset: lotDecOne),
    (SY:  335; SM: 12; SD: 31; EY:  336; EM:  1; ED: 29; DayOffset: lotIncOne),
    (SY:  336; SM:  2; SD: 28; EY:  336; EM:  3; ED: 28; DayOffset: lotIncOne),
    (SY:  336; SM:  4; SD: 27; EY:  336; EM:  5; ED: 26; DayOffset: lotIncOne),
    (SY:  336; SM:  6; SD: 25; EY:  336; EM:  7; ED: 24; DayOffset: lotDecOne),
    (SY:  336; SM:  8; SD: 23; EY:  336; EM:  9; ED: 21; DayOffset: lotDecOne),
    (SY:  336; SM: 10; SD: 21; EY:  336; EM: 11; ED: 19; DayOffset: lotDecOne),
    (SY:  337; SM:  1; SD: 18; EY:  337; EM:  2; ED: 16; DayOffset: lotIncOne),
    (SY:  337; SM:  3; SD: 18; EY:  337; EM:  4; ED: 16; DayOffset: lotIncOne),
    (SY:  337; SM:  5; SD: 16; EY:  337; EM:  6; ED: 14; DayOffset: lotIncOne),
    (SY:  337; SM: 10; SD: 10; EY:  337; EM: 12; ED:  8; DayOffset: lotDecOne),
    (SY:  338; SM:  4; SD:  6; EY:  338; EM:  5; ED:  5; DayOffset: lotIncOne),
    (SY:  338; SM:  6; SD:  4; EY:  338; EM:  7; ED:  3; DayOffset: lotIncOne),
    (SY:  339; SM:  1; SD: 26; EY:  339; EM:  2; ED: 24; DayOffset: lotDecOne),
    (SY:  339; SM:  6; SD: 23; EY:  339; EM:  7; ED: 22; DayOffset: lotIncOne),
    (SY:  341; SM:  8; SD: 28; EY:  341; EM:  9; ED: 26; DayOffset: lotIncOne),
    (SY:  342; SM:  4; SD: 21; EY:  342; EM:  5; ED: 20; DayOffset: lotDecOne),
    (SY:  342; SM:  6; SD: 19; EY:  342; EM:  7; ED: 18; DayOffset: lotDecOne),
    (SY:  342; SM: 11; SD: 14; EY:  342; EM: 12; ED: 13; DayOffset: lotIncOne),
    (SY:  343; SM:  5; SD: 10; EY:  343; EM:  6; ED:  8; DayOffset: lotDecOne),
    (SY:  343; SM:  7; SD:  8; EY:  343; EM:  8; ED:  6; DayOffset: lotDecOne),
    (SY:  343; SM:  9; SD:  5; EY:  343; EM: 10; ED:  4; DayOffset: lotDecOne),
    (SY:  343; SM: 12; SD:  3; EY:  344; EM:  1; ED:  1; DayOffset: lotIncOne),
    (SY:  344; SM:  1; SD: 31; EY:  344; EM:  2; ED: 29; DayOffset: lotIncOne),
    (SY:  344; SM:  3; SD: 30; EY:  344; EM:  4; ED: 28; DayOffset: lotIncOne),
    (SY:  344; SM:  5; SD: 28; EY:  344; EM:  6; ED: 26; DayOffset: lotDecOne),
    (SY:  344; SM:  7; SD: 26; EY:  344; EM:  8; ED: 24; DayOffset: lotDecOne),
    (SY:  344; SM:  9; SD: 23; EY:  344; EM: 10; ED: 22; DayOffset: lotDecOne),
    (SY:  344; SM: 12; SD: 21; EY:  345; EM:  1; ED: 19; DayOffset: lotIncOne),
    (SY:  345; SM:  2; SD: 18; EY:  345; EM:  3; ED: 19; DayOffset: lotIncOne),
    (SY:  345; SM:  4; SD: 18; EY:  345; EM:  5; ED: 17; DayOffset: lotIncOne),
    (SY:  345; SM:  9; SD: 12; EY:  345; EM: 11; ED: 10; DayOffset: lotDecOne),
    (SY:  345; SM: 12; SD: 10; EY:  346; EM:  1; ED:  8; DayOffset: lotDecOne),
    (SY:  346; SM:  3; SD:  9; EY:  346; EM:  4; ED:  7; DayOffset: lotIncOne),
    (SY:  346; SM:  5; SD:  7; EY:  346; EM:  6; ED:  5; DayOffset: lotIncOne),
    (SY:  346; SM: 10; SD:  1; EY:  346; EM: 10; ED: 30; DayOffset: lotDecOne),
    (SY:  346; SM: 12; SD: 29; EY:  347; EM:  1; ED: 27; DayOffset: lotDecOne),
    (SY:  347; SM:  3; SD: 28; EY:  347; EM:  4; ED: 26; DayOffset: lotIncOne),
    (SY:  347; SM:  5; SD: 26; EY:  347; EM:  6; ED: 24; DayOffset: lotIncOne),
    (SY:  347; SM: 10; SD: 20; EY:  347; EM: 11; ED: 18; DayOffset: lotDecOne),
    (SY:  348; SM:  3; SD: 16; EY:  348; EM:  4; ED: 14; DayOffset: lotIncOne),
    (SY:  349; SM:  6; SD:  2; EY:  349; EM:  7; ED:  1; DayOffset: lotIncOne),
    (SY:  349; SM:  7; SD: 31; EY:  349; EM:  8; ED: 29; DayOffset: lotIncOne),
    (SY:  350; SM:  3; SD: 24; EY:  350; EM:  4; ED: 22; DayOffset: lotDecOne),
    (SY:  350; SM: 10; SD: 17; EY:  350; EM: 11; ED: 15; DayOffset: lotIncOne),
    (SY:  351; SM:  4; SD: 12; EY:  351; EM:  5; ED: 11; DayOffset: lotDecOne),
    (SY:  351; SM:  6; SD: 10; EY:  351; EM:  7; ED:  9; DayOffset: lotDecOne),
    (SY:  351; SM:  8; SD:  8; EY:  351; EM:  9; ED:  6; DayOffset: lotDecOne),
    (SY:  351; SM: 11; SD:  5; EY:  351; EM: 12; ED:  4; DayOffset: lotIncOne),
    (SY:  352; SM:  1; SD:  3; EY:  352; EM:  2; ED:  1; DayOffset: lotIncOne),
    (SY:  352; SM:  3; SD:  2; EY:  352; EM:  3; ED: 31; DayOffset: lotIncOne),
    (SY:  352; SM:  4; SD: 30; EY:  352; EM:  5; ED: 29; DayOffset: lotDecOne),
    (SY:  352; SM:  6; SD: 28; EY:  352; EM:  7; ED: 27; DayOffset: lotDecOne),
    (SY:  352; SM:  8; SD: 26; EY:  352; EM:  9; ED: 24; DayOffset: lotDecOne),
    (SY:  353; SM:  1; SD: 21; EY:  353; EM:  2; ED: 19; DayOffset: lotIncOne),
    (SY:  353; SM:  3; SD: 21; EY:  353; EM:  4; ED: 19; DayOffset: lotIncOne),
    (SY:  353; SM:  8; SD: 15; EY:  353; EM: 10; ED: 13; DayOffset: lotDecOne),
    (SY:  353; SM: 11; SD: 12; EY:  353; EM: 12; ED: 11; DayOffset: lotDecOne),
    (SY:  354; SM:  2; SD:  9; EY:  354; EM:  3; ED: 10; DayOffset: lotIncOne),
    (SY:  354; SM:  4; SD:  9; EY:  354; EM:  5; ED:  8; DayOffset: lotIncOne),
    (SY:  354; SM:  9; SD:  3; EY:  354; EM: 10; ED:  2; DayOffset: lotDecOne),
    (SY:  354; SM: 11; SD:  1; EY:  354; EM: 12; ED: 30; DayOffset: lotDecOne),
    (SY:  355; SM:  2; SD: 28; EY:  355; EM:  3; ED: 29; DayOffset: lotIncOne),
    (SY:  355; SM:  4; SD: 28; EY:  355; EM:  5; ED: 27; DayOffset: lotIncOne),
    (SY:  355; SM:  9; SD: 22; EY:  355; EM: 10; ED: 21; DayOffset: lotDecOne),
    (SY:  355; SM: 11; SD: 20; EY:  355; EM: 12; ED: 19; DayOffset: lotDecOne),
    (SY:  356; SM:  2; SD: 17; EY:  356; EM:  3; ED: 17; DayOffset: lotIncOne),
    (SY:  356; SM:  5; SD: 16; EY:  356; EM:  6; ED: 14; DayOffset: lotIncOne),
    (SY:  356; SM: 10; SD: 10; EY:  356; EM: 11; ED:  8; DayOffset: lotDecOne),
    (SY:  356; SM: 12; SD:  8; EY:  357; EM:  1; ED:  6; DayOffset: lotDecOne),
    (SY:  357; SM:  5; SD:  5; EY:  357; EM:  6; ED:  3; DayOffset: lotIncOne),
    (SY:  357; SM:  7; SD:  3; EY:  357; EM:  8; ED:  1; DayOffset: lotIncOne),
    (SY:  357; SM: 12; SD: 27; EY:  358; EM:  1; ED: 25; DayOffset: lotDecOne),
    (SY:  358; SM:  2; SD: 24; EY:  358; EM:  3; ED: 25; DayOffset: lotDecOne),
    (SY:  358; SM:  7; SD: 22; EY:  358; EM:  8; ED: 20; DayOffset: lotIncOne),
    (SY:  358; SM:  9; SD: 19; EY:  358; EM: 10; ED: 18; DayOffset: lotIncOne),
    (SY:  359; SM:  3; SD: 15; EY:  359; EM:  4; ED: 13; DayOffset: lotDecOne),
    (SY:  359; SM:  5; SD: 13; EY:  359; EM:  6; ED: 11; DayOffset: lotDecOne),
    (SY:  359; SM: 10; SD:  8; EY:  359; EM: 11; ED:  6; DayOffset: lotIncOne),
    (SY:  359; SM: 12; SD:  6; EY:  360; EM:  1; ED:  4; DayOffset: lotIncOne),
    (SY:  360; SM:  2; SD:  3; EY:  360; EM:  3; ED:  3; DayOffset: lotIncOne),
    (SY:  360; SM:  4; SD:  2; EY:  360; EM:  5; ED:  1; DayOffset: lotDecOne),
    (SY:  360; SM:  5; SD: 31; EY:  360; EM:  6; ED: 29; DayOffset: lotDecOne),
    (SY:  360; SM:  7; SD: 29; EY:  360; EM:  8; ED: 27; DayOffset: lotDecOne),
    (SY:  360; SM: 12; SD: 24; EY:  361; EM:  1; ED: 22; DayOffset: lotIncOne),
    (SY:  361; SM:  2; SD: 21; EY:  361; EM:  3; ED: 22; DayOffset: lotIncOne),
    (SY:  361; SM:  7; SD: 18; EY:  361; EM:  9; ED: 15; DayOffset: lotDecOne),
    (SY:  361; SM: 10; SD: 15; EY:  361; EM: 11; ED: 13; DayOffset: lotDecOne),
    (SY:  362; SM:  1; SD: 12; EY:  362; EM:  2; ED: 10; DayOffset: lotIncOne),
    (SY:  362; SM:  3; SD: 12; EY:  362; EM:  4; ED: 10; DayOffset: lotIncOne),
    (SY:  362; SM:  8; SD:  6; EY:  362; EM:  9; ED:  4; DayOffset: lotDecOne),
    (SY:  362; SM: 10; SD:  4; EY:  362; EM: 12; ED:  2; DayOffset: lotDecOne),
    (SY:  363; SM:  1; SD: 31; EY:  363; EM:  3; ED:  1; DayOffset: lotIncOne),
    (SY:  363; SM:  3; SD: 31; EY:  363; EM:  4; ED: 29; DayOffset: lotIncOne),
    (SY:  363; SM:  8; SD: 25; EY:  363; EM:  9; ED: 23; DayOffset: lotDecOne),
    (SY:  363; SM: 10; SD: 23; EY:  363; EM: 11; ED: 21; DayOffset: lotDecOne),
    (SY:  364; SM:  1; SD: 20; EY:  364; EM:  2; ED: 18; DayOffset: lotIncOne),
    (SY:  364; SM:  4; SD: 18; EY:  364; EM:  5; ED: 17; DayOffset: lotIncOne),
    (SY:  364; SM:  9; SD: 12; EY:  364; EM: 10; ED: 11; DayOffset: lotDecOne),
    (SY:  364; SM: 11; SD: 10; EY:  364; EM: 12; ED:  9; DayOffset: lotDecOne),
    (SY:  365; SM:  4; SD:  7; EY:  365; EM:  5; ED:  6; DayOffset: lotIncOne),
    (SY:  365; SM:  6; SD:  5; EY:  365; EM:  7; ED:  4; DayOffset: lotIncOne),
    (SY:  365; SM: 10; SD:  1; EY:  365; EM: 10; ED: 30; DayOffset: lotDecOne),
    (SY:  365; SM: 11; SD: 29; EY:  365; EM: 12; ED: 28; DayOffset: lotDecOne),
    (SY:  366; SM:  1; SD: 27; EY:  366; EM:  2; ED: 25; DayOffset: lotDecOne),
    (SY:  366; SM:  6; SD: 24; EY:  366; EM:  7; ED: 23; DayOffset: lotIncOne),
    (SY:  366; SM:  8; SD: 22; EY:  366; EM:  9; ED: 20; DayOffset: lotIncOne),
    (SY:  366; SM: 10; SD: 20; EY:  366; EM: 11; ED: 18; DayOffset: lotDecOne),
    (SY:  366; SM: 12; SD: 18; EY:  367; EM:  1; ED: 16; DayOffset: lotDecOne),
    (SY:  367; SM:  2; SD: 15; EY:  367; EM:  3; ED: 16; DayOffset: lotDecOne),
    (SY:  367; SM:  9; SD: 10; EY:  367; EM: 10; ED:  9; DayOffset: lotIncOne),
    (SY:  367; SM: 11; SD:  8; EY:  367; EM: 12; ED:  7; DayOffset: lotIncOne),
    (SY:  368; SM:  3; SD:  5; EY:  368; EM:  4; ED:  3; DayOffset: lotDecOne),
    (SY:  368; SM:  5; SD:  3; EY:  368; EM:  6; ED:  1; DayOffset: lotDecOne),
    (SY:  368; SM:  7; SD:  1; EY:  368; EM:  7; ED: 30; DayOffset: lotDecOne),
    (SY:  368; SM: 11; SD: 26; EY:  368; EM: 12; ED: 25; DayOffset: lotIncOne),
    (SY:  369; SM:  1; SD: 24; EY:  369; EM:  2; ED: 22; DayOffset: lotIncOne),
    (SY:  369; SM:  7; SD: 20; EY:  369; EM:  8; ED: 18; DayOffset: lotDecOne),
    (SY:  369; SM: 12; SD: 15; EY:  370; EM:  1; ED: 13; DayOffset: lotIncOne),
    (SY:  370; SM:  2; SD: 12; EY:  370; EM:  3; ED: 13; DayOffset: lotIncOne),
    (SY:  370; SM:  7; SD:  9; EY:  370; EM:  8; ED:  7; DayOffset: lotDecOne),
    (SY:  371; SM:  1; SD:  3; EY:  371; EM:  2; ED:  1; DayOffset: lotIncOne),
    (SY:  371; SM:  3; SD:  3; EY:  371; EM:  4; ED:  1; DayOffset: lotIncOne),
    (SY:  371; SM:  7; SD: 28; EY:  371; EM:  8; ED: 26; DayOffset: lotDecOne),
    (SY:  371; SM:  9; SD: 25; EY:  371; EM: 10; ED: 24; DayOffset: lotDecOne),
    (SY:  371; SM: 12; SD: 23; EY:  372; EM:  1; ED: 21; DayOffset: lotIncOne),
    (SY:  372; SM:  3; SD: 21; EY:  372; EM:  4; ED: 19; DayOffset: lotIncOne),
    (SY:  372; SM:  8; SD: 15; EY:  372; EM:  9; ED: 13; DayOffset: lotDecOne),
    (SY:  372; SM: 10; SD: 13; EY:  372; EM: 11; ED: 11; DayOffset: lotDecOne),
    (SY:  373; SM:  3; SD: 10; EY:  373; EM:  4; ED:  8; DayOffset: lotIncOne),
    (SY:  373; SM:  5; SD:  8; EY:  373; EM:  6; ED:  6; DayOffset: lotIncOne),
    (SY:  373; SM:  9; SD:  3; EY:  373; EM: 10; ED:  2; DayOffset: lotDecOne),
    (SY:  373; SM: 11; SD:  1; EY:  373; EM: 11; ED: 30; DayOffset: lotDecOne),
    (SY:  373; SM: 12; SD: 30; EY:  374; EM:  1; ED: 28; DayOffset: lotDecOne),
    (SY:  374; SM:  3; SD: 29; EY:  374; EM:  4; ED: 27; DayOffset: lotIncOne),
    (SY:  374; SM:  5; SD: 27; EY:  374; EM:  6; ED: 25; DayOffset: lotIncOne),
    (SY:  374; SM:  7; SD: 25; EY:  374; EM:  8; ED: 23; DayOffset: lotIncOne),
    (SY:  374; SM: 11; SD: 20; EY:  374; EM: 12; ED: 19; DayOffset: lotDecOne),
    (SY:  375; SM:  1; SD: 18; EY:  375; EM:  2; ED: 16; DayOffset: lotDecOne),
    (SY:  375; SM:  6; SD: 15; EY:  375; EM:  7; ED: 14; DayOffset: lotIncOne),
    (SY:  375; SM:  8; SD: 13; EY:  375; EM:  9; ED: 11; DayOffset: lotIncOne),
    (SY:  376; SM:  2; SD:  6; EY:  376; EM:  3; ED:  6; DayOffset: lotDecOne),
    (SY:  376; SM:  4; SD:  5; EY:  376; EM:  5; ED:  4; DayOffset: lotDecOne),
    (SY:  376; SM: 10; SD: 29; EY:  376; EM: 11; ED: 27; DayOffset: lotIncOne),
    (SY:  377; SM:  6; SD: 22; EY:  377; EM:  7; ED: 21; DayOffset: lotDecOne),
    (SY:  377; SM: 11; SD: 17; EY:  377; EM: 12; ED: 16; DayOffset: lotIncOne),
    (SY:  378; SM:  1; SD: 15; EY:  378; EM:  2; ED: 13; DayOffset: lotIncOne),
    (SY:  378; SM:  6; SD: 11; EY:  378; EM:  7; ED: 10; DayOffset: lotDecOne),
    (SY:  379; SM:  2; SD:  3; EY:  379; EM:  3; ED:  4; DayOffset: lotIncOne),
    (SY:  379; SM:  6; SD: 30; EY:  379; EM:  7; ED: 29; DayOffset: lotDecOne),
    (SY:  379; SM:  8; SD: 28; EY:  379; EM:  9; ED: 26; DayOffset: lotDecOne),
    (SY:  379; SM: 11; SD: 25; EY:  379; EM: 12; ED: 24; DayOffset: lotIncOne),
    (SY:  380; SM:  2; SD: 22; EY:  380; EM:  3; ED: 22; DayOffset: lotIncOne),
    (SY:  380; SM:  7; SD: 18; EY:  380; EM:  8; ED: 16; DayOffset: lotDecOne),
    (SY:  380; SM:  9; SD: 15; EY:  380; EM: 10; ED: 14; DayOffset: lotDecOne),
    (SY:  381; SM:  2; SD: 10; EY:  381; EM:  5; ED:  9; DayOffset: lotIncOne),
    (SY:  381; SM:  8; SD:  6; EY:  381; EM:  9; ED:  4; DayOffset: lotDecOne),
    (SY:  381; SM: 10; SD:  4; EY:  381; EM: 11; ED:  2; DayOffset: lotDecOne),
    (SY:  381; SM: 12; SD:  2; EY:  381; EM: 12; ED: 31; DayOffset: lotDecOne),
    (SY:  382; SM:  3; SD:  1; EY:  382; EM:  3; ED: 30; DayOffset: lotIncOne),
    (SY:  382; SM:  4; SD: 29; EY:  382; EM:  5; ED: 28; DayOffset: lotIncOne),
    (SY:  382; SM:  6; SD: 27; EY:  382; EM:  7; ED: 26; DayOffset: lotIncOne),
    (SY:  382; SM: 10; SD: 23; EY:  382; EM: 11; ED: 21; DayOffset: lotDecOne),
    (SY:  382; SM: 12; SD: 21; EY:  383; EM:  1; ED: 19; DayOffset: lotDecOne),
    (SY:  383; SM:  5; SD: 18; EY:  383; EM:  6; ED: 16; DayOffset: lotIncOne),
    (SY:  383; SM:  7; SD: 16; EY:  383; EM:  8; ED: 14; DayOffset: lotIncOne),
    (SY:  384; SM:  1; SD:  9; EY:  384; EM:  2; ED:  7; DayOffset: lotDecOne),
    (SY:  384; SM:  3; SD:  8; EY:  384; EM:  4; ED:  6; DayOffset: lotDecOne),
    (SY:  385; SM:  5; SD: 25; EY:  385; EM:  6; ED: 23; DayOffset: lotDecOne),
    (SY:  387; SM:  6; SD:  2; EY:  387; EM:  7; ED:  1; DayOffset: lotDecOne),
    (SY:  387; SM:  7; SD: 31; EY:  387; EM:  8; ED: 29; DayOffset: lotDecOne),
    (SY:  387; SM: 10; SD: 28; EY:  387; EM: 11; ED: 26; DayOffset: lotIncOne),
    (SY:  388; SM:  1; SD: 25; EY:  388; EM:  2; ED: 23; DayOffset: lotIncOne),
    (SY:  388; SM:  6; SD: 20; EY:  388; EM:  7; ED: 19; DayOffset: lotDecOne),
    (SY:  388; SM:  8; SD: 18; EY:  388; EM:  9; ED: 16; DayOffset: lotDecOne),
    (SY:  389; SM:  1; SD: 13; EY:  389; EM:  4; ED: 11; DayOffset: lotIncOne),
    (SY:  389; SM:  7; SD:  9; EY:  389; EM:  8; ED:  7; DayOffset: lotDecOne),
    (SY:  389; SM:  9; SD:  6; EY:  389; EM: 10; ED:  5; DayOffset: lotDecOne),
    (SY:  389; SM: 11; SD:  4; EY:  389; EM: 12; ED:  3; DayOffset: lotDecOne),
    (SY:  390; SM:  2; SD:  1; EY:  390; EM:  3; ED:  2; DayOffset: lotIncOne),
    (SY:  390; SM:  4; SD:  1; EY:  390; EM:  4; ED: 30; DayOffset: lotIncOne),
    (SY:  390; SM:  5; SD: 30; EY:  390; EM:  6; ED: 28; DayOffset: lotIncOne),
    (SY:  390; SM:  9; SD: 25; EY:  390; EM: 10; ED: 24; DayOffset: lotDecOne),
    (SY:  390; SM: 11; SD: 23; EY:  390; EM: 12; ED: 22; DayOffset: lotDecOne),
    (SY:  391; SM:  2; SD: 20; EY:  391; EM:  3; ED: 21; DayOffset: lotIncOne),
    (SY:  391; SM:  4; SD: 20; EY:  391; EM:  5; ED: 19; DayOffset: lotIncOne),
    (SY:  391; SM:  6; SD: 18; EY:  391; EM:  7; ED: 17; DayOffset: lotIncOne),
    (SY:  391; SM: 12; SD: 12; EY:  392; EM:  1; ED: 10; DayOffset: lotDecOne),
    (SY:  392; SM:  2; SD:  9; EY:  392; EM:  3; ED:  9; DayOffset: lotDecOne),
    (SY:  392; SM:  5; SD:  8; EY:  392; EM:  6; ED:  6; DayOffset: lotIncOne),
    (SY:  392; SM:  7; SD:  6; EY:  392; EM:  8; ED:  4; DayOffset: lotIncOne),
    (SY:  394; SM:  7; SD: 14; EY:  394; EM:  8; ED: 12; DayOffset: lotIncOne),
    (SY:  395; SM:  5; SD:  5; EY:  395; EM:  6; ED:  3; DayOffset: lotDecOne),
    (SY:  395; SM:  9; SD: 30; EY:  395; EM: 10; ED: 29; DayOffset: lotIncOne),
    (SY:  396; SM:  5; SD: 23; EY:  396; EM:  6; ED: 21; DayOffset: lotDecOne),
    (SY:  396; SM:  7; SD: 21; EY:  396; EM:  8; ED: 19; DayOffset: lotDecOne),
    (SY:  396; SM: 12; SD: 16; EY:  397; EM:  1; ED: 14; DayOffset: lotIncOne),
    (SY:  397; SM:  2; SD: 13; EY:  397; EM:  3; ED: 14; DayOffset: lotIncOne),
    (SY:  397; SM:  6; SD: 11; EY:  397; EM:  7; ED: 10; DayOffset: lotDecOne),
    (SY:  397; SM:  8; SD:  9; EY:  397; EM:  9; ED:  7; DayOffset: lotDecOne),
    (SY:  397; SM: 10; SD:  7; EY:  397; EM: 11; ED:  5; DayOffset: lotDecOne),
    (SY:  398; SM:  1; SD:  4; EY:  398; EM:  2; ED:  2; DayOffset: lotIncOne),
    (SY:  398; SM:  3; SD:  4; EY:  398; EM:  4; ED:  2; DayOffset: lotIncOne),
    (SY:  398; SM:  5; SD:  2; EY:  398; EM:  5; ED: 31; DayOffset: lotIncOne),
    (SY:  398; SM:  8; SD: 28; EY:  398; EM:  9; ED: 26; DayOffset: lotDecOne),
    (SY:  398; SM: 10; SD: 26; EY:  398; EM: 11; ED: 24; DayOffset: lotDecOne),
    (SY:  399; SM:  1; SD: 23; EY:  399; EM:  2; ED: 21; DayOffset: lotIncOne),
    (SY:  399; SM:  3; SD: 23; EY:  399; EM:  4; ED: 21; DayOffset: lotIncOne),
    (SY:  399; SM:  5; SD: 21; EY:  399; EM:  6; ED: 19; DayOffset: lotIncOne),
    (SY:  399; SM: 10; SD: 15; EY:  399; EM: 12; ED: 13; DayOffset: lotDecOne),
    (SY:  400; SM:  1; SD: 12; EY:  400; EM:  2; ED: 10; DayOffset: lotDecOne),
    (SY:  400; SM:  4; SD: 10; EY:  400; EM:  5; ED:  9; DayOffset: lotIncOne),
    (SY:  400; SM:  6; SD:  8; EY:  400; EM:  7; ED:  7; DayOffset: lotIncOne),
    (SY:  400; SM: 11; SD:  2; EY:  400; EM: 12; ED:  1; DayOffset: lotDecOne),
    (SY:  402; SM:  6; SD: 16; EY:  402; EM:  7; ED: 15; DayOffset: lotIncOne),
    (SY:  403; SM:  9; SD:  2; EY:  403; EM: 10; ED:  1; DayOffset: lotIncOne),
    (SY:  404; SM:  4; SD: 25; EY:  404; EM:  5; ED: 24; DayOffset: lotDecOne),
    (SY:  404; SM:  6; SD: 23; EY:  404; EM:  7; ED: 22; DayOffset: lotDecOne),
    (SY:  404; SM: 11; SD: 18; EY:  404; EM: 12; ED: 17; DayOffset: lotIncOne),
    (SY:  405; SM:  1; SD: 16; EY:  405; EM:  2; ED: 14; DayOffset: lotIncOne),
    (SY:  405; SM:  5; SD: 14; EY:  405; EM:  6; ED: 12; DayOffset: lotDecOne),
    (SY:  405; SM:  7; SD: 12; EY:  405; EM:  8; ED: 10; DayOffset: lotDecOne),
    (SY:  405; SM:  9; SD:  9; EY:  405; EM: 10; ED:  8; DayOffset: lotDecOne),
    (SY:  405; SM: 12; SD:  7; EY:  406; EM:  1; ED:  5; DayOffset: lotIncOne),
    (SY:  406; SM:  2; SD:  4; EY:  406; EM:  3; ED:  5; DayOffset: lotIncOne),
    (SY:  406; SM:  4; SD:  4; EY:  406; EM:  5; ED:  3; DayOffset: lotIncOne),
    (SY:  406; SM:  7; SD: 31; EY:  406; EM:  8; ED: 29; DayOffset: lotDecOne),
    (SY:  406; SM:  9; SD: 28; EY:  406; EM: 10; ED: 27; DayOffset: lotDecOne),
    (SY:  407; SM:  2; SD: 23; EY:  407; EM:  3; ED: 24; DayOffset: lotIncOne),
    (SY:  407; SM:  4; SD: 23; EY:  407; EM:  5; ED: 22; DayOffset: lotIncOne),
    (SY:  407; SM:  9; SD: 17; EY:  407; EM: 11; ED: 15; DayOffset: lotDecOne),
    (SY:  407; SM: 12; SD: 15; EY:  408; EM:  1; ED: 13; DayOffset: lotDecOne),
    (SY:  408; SM:  3; SD: 13; EY:  408; EM:  4; ED: 11; DayOffset: lotIncOne),
    (SY:  408; SM:  5; SD: 11; EY:  408; EM:  6; ED:  9; DayOffset: lotIncOne),
    (SY:  408; SM: 10; SD:  5; EY:  408; EM: 11; ED:  3; DayOffset: lotDecOne),
    (SY:  409; SM:  5; SD: 30; EY:  409; EM:  6; ED: 28; DayOffset: lotIncOne),
    (SY:  409; SM: 10; SD: 24; EY:  409; EM: 11; ED: 22; DayOffset: lotDecOne),
    (SY:  409; SM: 12; SD: 22; EY:  410; EM:  1; ED: 20; DayOffset: lotDecOne),
    (SY:  410; SM:  3; SD: 21; EY:  410; EM:  4; ED: 19; DayOffset: lotIncOne),
    (SY:  410; SM:  5; SD: 19; EY:  410; EM:  6; ED: 17; DayOffset: lotIncOne),
    (SY:  411; SM:  1; SD: 10; EY:  411; EM:  2; ED:  8; DayOffset: lotDecOne),
    (SY:  411; SM:  8; SD:  5; EY:  411; EM:  9; ED:  3; DayOffset: lotIncOne),
    (SY:  412; SM:  3; SD: 28; EY:  412; EM:  4; ED: 26; DayOffset: lotDecOne),
    (SY:  412; SM: 10; SD: 21; EY:  412; EM: 11; ED: 19; DayOffset: lotIncOne),
    (SY:  412; SM: 12; SD: 19; EY:  413; EM:  1; ED: 17; DayOffset: lotIncOne),
    (SY:  413; SM:  4; SD: 16; EY:  413; EM:  5; ED: 15; DayOffset: lotDecOne),
    (SY:  413; SM:  6; SD: 14; EY:  413; EM:  7; ED: 13; DayOffset: lotDecOne),
    (SY:  413; SM:  8; SD: 12; EY:  413; EM:  9; ED: 10; DayOffset: lotDecOne),
    (SY:  413; SM: 11; SD:  9; EY:  413; EM: 12; ED:  8; DayOffset: lotIncOne),
    (SY:  414; SM:  1; SD:  7; EY:  414; EM:  2; ED:  5; DayOffset: lotIncOne),
    (SY:  414; SM:  3; SD:  7; EY:  414; EM:  4; ED:  5; DayOffset: lotIncOne),
    (SY:  414; SM:  7; SD:  3; EY:  414; EM:  8; ED:  1; DayOffset: lotDecOne),
    (SY:  414; SM:  8; SD: 31; EY:  414; EM:  9; ED: 29; DayOffset: lotDecOne),
    (SY:  415; SM:  1; SD: 26; EY:  415; EM:  2; ED: 24; DayOffset: lotIncOne),
    (SY:  415; SM:  3; SD: 26; EY:  415; EM:  4; ED: 24; DayOffset: lotIncOne),
    (SY:  415; SM:  8; SD: 20; EY:  415; EM: 10; ED: 18; DayOffset: lotDecOne),
    (SY:  415; SM: 11; SD: 17; EY:  415; EM: 12; ED: 16; DayOffset: lotDecOne),
    (SY:  416; SM:  2; SD: 14; EY:  416; EM:  3; ED: 14; DayOffset: lotIncOne),
    (SY:  416; SM:  4; SD: 13; EY:  416; EM:  5; ED: 12; DayOffset: lotIncOne),
    (SY:  416; SM:  9; SD:  7; EY:  416; EM: 10; ED:  6; DayOffset: lotDecOne),
    (SY:  416; SM: 11; SD:  5; EY:  416; EM: 12; ED:  4; DayOffset: lotDecOne),
    (SY:  417; SM:  3; SD:  4; EY:  417; EM:  4; ED:  2; DayOffset: lotIncOne),
    (SY:  417; SM:  5; SD:  2; EY:  417; EM:  5; ED: 31; DayOffset: lotIncOne),
    (SY:  417; SM:  9; SD: 26; EY:  417; EM: 10; ED: 25; DayOffset: lotDecOne),
    (SY:  417; SM: 11; SD: 24; EY:  417; EM: 12; ED: 23; DayOffset: lotDecOne),
    (SY:  418; SM:  2; SD: 21; EY:  418; EM:  3; ED: 22; DayOffset: lotIncOne),
    (SY:  418; SM:  4; SD: 21; EY:  418; EM:  5; ED: 20; DayOffset: lotIncOne),
    (SY:  418; SM: 10; SD: 15; EY:  418; EM: 11; ED: 13; DayOffset: lotDecOne),
    (SY:  418; SM: 12; SD: 13; EY:  419; EM:  1; ED: 11; DayOffset: lotDecOne),
    (SY:  419; SM:  5; SD: 10; EY:  419; EM:  6; ED:  8; DayOffset: lotIncOne),
    (SY:  419; SM:  7; SD:  8; EY:  419; EM:  8; ED:  6; DayOffset: lotIncOne),
    (SY:  419; SM: 11; SD:  3; EY:  419; EM: 12; ED:  2; DayOffset: lotDecOne),
    (SY:  420; SM:  1; SD:  1; EY:  420; EM:  1; ED: 30; DayOffset: lotDecOne),
    (SY:  420; SM:  2; SD: 29; EY:  420; EM:  3; ED: 29; DayOffset: lotDecOne),
    (SY:  420; SM:  7; SD: 26; EY:  420; EM:  8; ED: 24; DayOffset: lotIncOne),
    (SY:  420; SM:  9; SD: 23; EY:  420; EM: 10; ED: 22; DayOffset: lotIncOne),
    (SY:  420; SM: 11; SD: 21; EY:  420; EM: 12; ED: 20; DayOffset: lotIncOne),
    (SY:  421; SM:  1; SD: 19; EY:  421; EM:  2; ED: 17; DayOffset: lotDecOne),
    (SY:  421; SM:  3; SD: 19; EY:  421; EM:  4; ED: 17; DayOffset: lotDecOne),
    (SY:  421; SM:  5; SD: 17; EY:  421; EM:  6; ED: 15; DayOffset: lotDecOne),
    (SY:  421; SM: 10; SD: 12; EY:  421; EM: 11; ED: 10; DayOffset: lotIncOne),
    (SY:  421; SM: 12; SD: 10; EY:  422; EM:  1; ED:  8; DayOffset: lotIncOne),
    (SY:  422; SM:  2; SD:  7; EY:  422; EM:  3; ED:  8; DayOffset: lotIncOne),
    (SY:  422; SM:  6; SD:  5; EY:  422; EM:  7; ED:  4; DayOffset: lotDecOne),
    (SY:  422; SM:  8; SD:  3; EY:  422; EM:  9; ED:  1; DayOffset: lotDecOne),
    (SY:  422; SM: 12; SD: 29; EY:  423; EM:  1; ED: 27; DayOffset: lotIncOne),
    (SY:  423; SM:  2; SD: 26; EY:  423; EM:  3; ED: 27; DayOffset: lotIncOne),
    (SY:  423; SM:  7; SD: 23; EY:  423; EM:  9; ED: 20; DayOffset: lotDecOne),
    (SY:  423; SM: 10; SD: 20; EY:  423; EM: 11; ED: 18; DayOffset: lotDecOne),
    (SY:  424; SM:  1; SD: 17; EY:  424; EM:  2; ED: 15; DayOffset: lotIncOne),
    (SY:  424; SM:  3; SD: 16; EY:  424; EM:  4; ED: 14; DayOffset: lotIncOne),
    (SY:  424; SM:  8; SD: 10; EY:  424; EM:  9; ED:  8; DayOffset: lotDecOne),
    (SY:  424; SM: 10; SD:  8; EY:  424; EM: 11; ED:  6; DayOffset: lotDecOne),
    (SY:  425; SM:  2; SD:  4; EY:  425; EM:  3; ED:  5; DayOffset: lotIncOne),
    (SY:  425; SM:  4; SD:  4; EY:  425; EM:  5; ED:  3; DayOffset: lotIncOne),
    (SY:  425; SM:  8; SD: 29; EY:  425; EM:  9; ED: 27; DayOffset: lotDecOne),
    (SY:  425; SM: 10; SD: 27; EY:  425; EM: 11; ED: 25; DayOffset: lotDecOne),
    (SY:  426; SM:  1; SD: 24; EY:  426; EM:  2; ED: 22; DayOffset: lotIncOne),
    (SY:  426; SM:  3; SD: 24; EY:  426; EM:  5; ED: 22; DayOffset: lotIncOne),
    (SY:  426; SM:  9; SD: 17; EY:  426; EM: 10; ED: 16; DayOffset: lotDecOne),
    (SY:  426; SM: 11; SD: 15; EY:  426; EM: 12; ED: 14; DayOffset: lotDecOne),
    (SY:  427; SM:  4; SD: 12; EY:  427; EM:  5; ED: 11; DayOffset: lotIncOne),
    (SY:  427; SM:  6; SD: 10; EY:  427; EM:  7; ED:  9; DayOffset: lotIncOne),
    (SY:  427; SM: 10; SD:  6; EY:  427; EM: 11; ED:  4; DayOffset: lotDecOne),
    (SY:  427; SM: 12; SD:  4; EY:  428; EM:  1; ED:  2; DayOffset: lotDecOne),
    (SY:  428; SM:  2; SD:  1; EY:  428; EM:  3; ED:  1; DayOffset: lotDecOne),
    (SY:  428; SM:  6; SD: 28; EY:  428; EM:  7; ED: 27; DayOffset: lotIncOne),
    (SY:  428; SM:  8; SD: 26; EY:  428; EM:  9; ED: 24; DayOffset: lotIncOne),
    (SY:  428; SM: 12; SD: 22; EY:  429; EM:  1; ED: 20; DayOffset: lotDecOne),
    (SY:  429; SM:  2; SD: 19; EY:  429; EM:  3; ED: 20; DayOffset: lotDecOne),
    (SY:  429; SM:  4; SD: 19; EY:  429; EM:  5; ED: 18; DayOffset: lotDecOne),
    (SY:  429; SM:  9; SD: 14; EY:  429; EM: 10; ED: 13; DayOffset: lotIncOne),
    (SY:  429; SM: 11; SD: 12; EY:  429; EM: 12; ED: 11; DayOffset: lotIncOne),
    (SY:  430; SM:  5; SD:  8; EY:  430; EM:  6; ED:  6; DayOffset: lotDecOne),
    (SY:  430; SM:  7; SD:  6; EY:  430; EM:  8; ED:  4; DayOffset: lotDecOne),
    (SY:  430; SM: 12; SD:  1; EY:  430; EM: 12; ED: 30; DayOffset: lotIncOne),
    (SY:  431; SM:  1; SD: 29; EY:  431; EM:  2; ED: 27; DayOffset: lotIncOne),
    (SY:  431; SM:  7; SD: 25; EY:  431; EM:  8; ED: 23; DayOffset: lotDecOne),
    (SY:  431; SM:  9; SD: 22; EY:  431; EM: 10; ED: 21; DayOffset: lotDecOne),
    (SY:  431; SM: 12; SD: 20; EY:  432; EM:  1; ED: 18; DayOffset: lotIncOne),
    (SY:  432; SM:  2; SD: 17; EY:  432; EM:  3; ED: 17; DayOffset: lotIncOne),
    (SY:  432; SM:  7; SD: 13; EY:  432; EM:  8; ED: 11; DayOffset: lotDecOne),
    (SY:  432; SM:  9; SD: 10; EY:  432; EM: 10; ED:  9; DayOffset: lotDecOne),
    (SY:  433; SM:  1; SD:  7; EY:  433; EM:  2; ED:  5; DayOffset: lotIncOne),
    (SY:  433; SM:  3; SD:  7; EY:  433; EM:  4; ED:  5; DayOffset: lotIncOne),
    (SY:  433; SM:  8; SD:  1; EY:  433; EM:  8; ED: 30; DayOffset: lotDecOne),
    (SY:  433; SM:  9; SD: 29; EY:  433; EM: 10; ED: 28; DayOffset: lotDecOne),
    (SY:  433; SM: 12; SD: 27; EY:  434; EM:  1; ED: 25; DayOffset: lotIncOne),
    (SY:  434; SM:  2; SD: 24; EY:  434; EM:  4; ED: 24; DayOffset: lotIncOne),
    (SY:  434; SM:  8; SD: 20; EY:  434; EM:  9; ED: 18; DayOffset: lotDecOne),
    (SY:  434; SM: 10; SD: 18; EY:  434; EM: 11; ED: 16; DayOffset: lotDecOne),
    (SY:  435; SM:  3; SD: 15; EY:  435; EM:  4; ED: 13; DayOffset: lotIncOne),
    (SY:  435; SM:  5; SD: 13; EY:  435; EM:  6; ED: 11; DayOffset: lotIncOne),
    (SY:  435; SM:  9; SD:  8; EY:  435; EM: 10; ED:  7; DayOffset: lotDecOne),
    (SY:  435; SM: 11; SD:  6; EY:  435; EM: 12; ED:  5; DayOffset: lotDecOne),
    (SY:  436; SM:  1; SD:  4; EY:  436; EM:  2; ED:  2; DayOffset: lotDecOne),
    (SY:  436; SM:  4; SD:  2; EY:  436; EM:  5; ED:  1; DayOffset: lotIncOne),
    (SY:  436; SM:  5; SD: 31; EY:  436; EM:  6; ED: 29; DayOffset: lotIncOne),
    (SY:  436; SM:  7; SD: 29; EY:  436; EM:  8; ED: 27; DayOffset: lotIncOne),
    (SY:  436; SM: 11; SD: 24; EY:  436; EM: 12; ED: 23; DayOffset: lotDecOne),
    (SY:  437; SM:  1; SD: 22; EY:  437; EM:  2; ED: 20; DayOffset: lotDecOne),
    (SY:  437; SM:  6; SD: 19; EY:  437; EM:  7; ED: 18; DayOffset: lotIncOne),
    (SY:  437; SM:  8; SD: 17; EY:  437; EM:  9; ED: 15; DayOffset: lotIncOne),
    (SY:  438; SM:  4; SD: 10; EY:  438; EM:  5; ED:  9; DayOffset: lotDecOne),
    (SY:  439; SM:  6; SD: 27; EY:  439; EM:  7; ED: 26; DayOffset: lotDecOne),
    (SY:  439; SM: 11; SD: 22; EY:  439; EM: 12; ED: 21; DayOffset: lotIncOne),
    (SY:  440; SM:  1; SD: 20; EY:  440; EM:  2; ED: 18; DayOffset: lotIncOne),
    (SY:  440; SM:  6; SD: 15; EY:  440; EM:  7; ED: 14; DayOffset: lotDecOne),
    (SY:  441; SM:  2; SD:  7; EY:  441; EM:  3; ED:  8; DayOffset: lotIncOne),
    (SY:  441; SM:  7; SD:  4; EY:  441; EM:  8; ED:  2; DayOffset: lotDecOne),
    (SY:  441; SM:  9; SD:  1; EY:  441; EM:  9; ED: 30; DayOffset: lotDecOne),
    (SY:  441; SM: 11; SD: 29; EY:  441; EM: 12; ED: 28; DayOffset: lotIncOne),
    (SY:  442; SM:  1; SD: 27; EY:  442; EM:  3; ED: 27; DayOffset: lotIncOne),
    (SY:  442; SM:  7; SD: 23; EY:  442; EM:  8; ED: 21; DayOffset: lotDecOne),
    (SY:  442; SM:  9; SD: 20; EY:  442; EM: 10; ED: 19; DayOffset: lotDecOne),
    (SY:  443; SM:  2; SD: 15; EY:  443; EM:  3; ED: 16; DayOffset: lotIncOne),
    (SY:  443; SM:  4; SD: 15; EY:  443; EM:  5; ED: 14; DayOffset: lotIncOne),
    (SY:  443; SM:  8; SD: 11; EY:  443; EM:  9; ED:  9; DayOffset: lotDecOne),
    (SY:  443; SM: 10; SD:  9; EY:  443; EM: 11; ED:  7; DayOffset: lotDecOne),
    (SY:  443; SM: 12; SD:  7; EY:  444; EM:  1; ED:  5; DayOffset: lotDecOne),
    (SY:  444; SM:  3; SD:  5; EY:  444; EM:  4; ED:  3; DayOffset: lotIncOne),
    (SY:  444; SM:  5; SD:  3; EY:  444; EM:  6; ED:  1; DayOffset: lotIncOne),
    (SY:  444; SM:  7; SD:  1; EY:  444; EM:  7; ED: 30; DayOffset: lotIncOne),
    (SY:  444; SM: 10; SD: 27; EY:  444; EM: 11; ED: 25; DayOffset: lotDecOne),
    (SY:  444; SM: 12; SD: 25; EY:  445; EM:  1; ED: 23; DayOffset: lotDecOne),
    (SY:  445; SM:  5; SD: 22; EY:  445; EM:  6; ED: 20; DayOffset: lotIncOne),
    (SY:  445; SM:  7; SD: 20; EY:  445; EM:  8; ED: 18; DayOffset: lotIncOne),
    (SY:  446; SM:  3; SD: 13; EY:  446; EM:  4; ED: 11; DayOffset: lotDecOne),
    (SY:  447; SM:  5; SD: 30; EY:  447; EM:  6; ED: 28; DayOffset: lotDecOne),
    (SY:  449; SM:  6; SD:  6; EY:  449; EM:  7; ED:  5; DayOffset: lotDecOne),
    (SY:  449; SM:  8; SD:  4; EY:  449; EM:  9; ED:  2; DayOffset: lotDecOne),
    (SY:  449; SM: 11; SD:  1; EY:  449; EM: 11; ED: 30; DayOffset: lotIncOne),
    (SY:  449; SM: 12; SD: 30; EY:  450; EM:  2; ED: 27; DayOffset: lotIncOne),
    (SY:  450; SM:  6; SD: 25; EY:  450; EM:  7; ED: 24; DayOffset: lotDecOne),
    (SY:  450; SM:  8; SD: 23; EY:  450; EM:  9; ED: 21; DayOffset: lotDecOne),
    (SY:  451; SM:  1; SD: 18; EY:  451; EM:  2; ED: 16; DayOffset: lotIncOne),
    (SY:  451; SM:  3; SD: 18; EY:  451; EM:  4; ED: 16; DayOffset: lotIncOne),
    (SY:  451; SM:  7; SD: 14; EY:  451; EM:  8; ED: 12; DayOffset: lotDecOne),
    (SY:  451; SM:  9; SD: 11; EY:  451; EM: 10; ED: 10; DayOffset: lotDecOne),
    (SY:  451; SM: 11; SD:  9; EY:  451; EM: 12; ED:  8; DayOffset: lotDecOne),
    (SY:  452; SM:  2; SD:  6; EY:  452; EM:  3; ED:  6; DayOffset: lotIncOne),
    (SY:  452; SM:  4; SD:  5; EY:  452; EM:  5; ED:  4; DayOffset: lotIncOne),
    (SY:  452; SM:  6; SD:  3; EY:  452; EM:  7; ED:  2; DayOffset: lotIncOne),
    (SY:  452; SM:  9; SD: 29; EY:  452; EM: 10; ED: 28; DayOffset: lotDecOne),
    (SY:  452; SM: 11; SD: 27; EY:  452; EM: 12; ED: 26; DayOffset: lotDecOne),
    (SY:  453; SM:  4; SD: 24; EY:  453; EM:  5; ED: 23; DayOffset: lotIncOne),
    (SY:  453; SM:  6; SD: 22; EY:  453; EM:  7; ED: 21; DayOffset: lotIncOne),
    (SY:  454; SM:  2; SD: 13; EY:  454; EM:  3; ED: 14; DayOffset: lotDecOne),
    (SY:  457; SM:  5; SD:  9; EY:  457; EM:  6; ED:  7; DayOffset: lotDecOne),
    (SY:  457; SM: 10; SD:  4; EY:  457; EM: 11; ED:  2; DayOffset: lotIncOne),
    (SY:  457; SM: 12; SD:  2; EY:  457; EM: 12; ED: 31; DayOffset: lotIncOne),
    (SY:  458; SM:  5; SD: 28; EY:  458; EM:  6; ED: 26; DayOffset: lotDecOne),
    (SY:  458; SM:  7; SD: 26; EY:  458; EM:  8; ED: 24; DayOffset: lotDecOne),
    (SY:  458; SM: 12; SD: 21; EY:  459; EM:  1; ED: 19; DayOffset: lotIncOne),
    (SY:  459; SM:  2; SD: 18; EY:  459; EM:  3; ED: 19; DayOffset: lotIncOne),
    (SY:  459; SM:  4; SD: 18; EY:  459; EM:  5; ED: 17; DayOffset: lotIncOne),
    (SY:  459; SM:  6; SD: 16; EY:  459; EM:  7; ED: 15; DayOffset: lotDecOne),
    (SY:  459; SM:  8; SD: 14; EY:  459; EM:  9; ED: 12; DayOffset: lotDecOne),
    (SY:  459; SM: 10; SD: 12; EY:  459; EM: 11; ED: 10; DayOffset: lotDecOne),
    (SY:  460; SM:  1; SD:  9; EY:  460; EM:  2; ED:  7; DayOffset: lotIncOne),
    (SY:  460; SM:  3; SD:  8; EY:  460; EM:  4; ED:  6; DayOffset: lotIncOne),
    (SY:  460; SM:  5; SD:  6; EY:  460; EM:  6; ED:  4; DayOffset: lotIncOne),
    (SY:  460; SM:  9; SD:  1; EY:  460; EM: 11; ED: 28; DayOffset: lotDecOne),
    (SY:  461; SM:  3; SD: 27; EY:  461; EM:  4; ED: 25; DayOffset: lotIncOne),
    (SY:  461; SM:  5; SD: 25; EY:  461; EM:  6; ED: 23; DayOffset: lotIncOne),
    (SY:  461; SM: 10; SD: 19; EY:  461; EM: 11; ED: 17; DayOffset: lotDecOne),
    (SY:  462; SM:  1; SD: 16; EY:  462; EM:  2; ED: 14; DayOffset: lotDecOne),
    (SY:  462; SM:  4; SD: 15; EY:  462; EM:  5; ED: 14; DayOffset: lotIncOne),
    (SY:  462; SM:  6; SD: 13; EY:  462; EM:  7; ED: 12; DayOffset: lotIncOne),
    (SY:  462; SM: 11; SD:  7; EY:  462; EM: 12; ED:  6; DayOffset: lotDecOne),
    (SY:  463; SM:  4; SD:  4; EY:  463; EM:  5; ED:  3; DayOffset: lotIncOne),
    (SY:  464; SM:  6; SD: 20; EY:  464; EM:  7; ED: 19; DayOffset: lotIncOne),
    (SY:  465; SM:  9; SD:  6; EY:  465; EM: 10; ED:  5; DayOffset: lotIncOne),
    (SY:  465; SM: 11; SD:  4; EY:  465; EM: 12; ED:  3; DayOffset: lotIncOne),
    (SY:  466; SM:  4; SD: 30; EY:  466; EM:  5; ED: 29; DayOffset: lotDecOne),
    (SY:  466; SM:  6; SD: 28; EY:  466; EM:  7; ED: 27; DayOffset: lotDecOne),
    (SY:  466; SM: 11; SD: 23; EY:  466; EM: 12; ED: 22; DayOffset: lotIncOne),
    (SY:  467; SM:  1; SD: 21; EY:  467; EM:  2; ED: 19; DayOffset: lotIncOne),
    (SY:  467; SM:  3; SD: 21; EY:  467; EM:  4; ED: 19; DayOffset: lotIncOne),
    (SY:  467; SM:  5; SD: 19; EY:  467; EM:  6; ED: 17; DayOffset: lotDecOne),
    (SY:  467; SM:  7; SD: 17; EY:  467; EM:  8; ED: 15; DayOffset: lotDecOne),
    (SY:  467; SM:  9; SD: 14; EY:  467; EM: 10; ED: 13; DayOffset: lotDecOne),
    (SY:  467; SM: 12; SD: 12; EY:  468; EM:  1; ED: 10; DayOffset: lotIncOne),
    (SY:  468; SM:  2; SD:  9; EY:  468; EM:  3; ED:  9; DayOffset: lotIncOne),
    (SY:  468; SM:  4; SD:  8; EY:  468; EM:  5; ED:  7; DayOffset: lotIncOne),
    (SY:  468; SM:  8; SD:  4; EY:  468; EM: 10; ED: 31; DayOffset: lotDecOne),
    (SY:  468; SM: 12; SD: 30; EY:  469; EM:  1; ED: 28; DayOffset: lotIncOne),
    (SY:  469; SM:  2; SD: 27; EY:  469; EM:  3; ED: 28; DayOffset: lotIncOne),
    (SY:  469; SM:  4; SD: 27; EY:  469; EM:  5; ED: 26; DayOffset: lotIncOne),
    (SY:  469; SM:  9; SD: 21; EY:  469; EM: 10; ED: 20; DayOffset: lotDecOne),
    (SY:  469; SM: 12; SD: 19; EY:  470; EM:  1; ED: 17; DayOffset: lotDecOne),
    (SY:  470; SM:  3; SD: 18; EY:  470; EM:  4; ED: 16; DayOffset: lotIncOne),
    (SY:  470; SM:  5; SD: 16; EY:  470; EM:  6; ED: 14; DayOffset: lotIncOne),
    (SY:  470; SM: 10; SD: 10; EY:  470; EM: 11; ED:  8; DayOffset: lotDecOne),
    (SY:  471; SM:  3; SD:  7; EY:  471; EM:  4; ED:  5; DayOffset: lotIncOne),
    (SY:  471; SM:  6; SD:  4; EY:  471; EM:  7; ED:  3; DayOffset: lotIncOne),
    (SY:  471; SM: 10; SD: 29; EY:  471; EM: 11; ED: 27; DayOffset: lotDecOne),
    (SY:  471; SM: 12; SD: 27; EY:  472; EM:  1; ED: 25; DayOffset: lotDecOne),
    (SY:  472; SM:  5; SD: 23; EY:  472; EM:  6; ED: 21; DayOffset: lotIncOne),
    (SY:  472; SM: 11; SD: 16; EY:  472; EM: 12; ED: 15; DayOffset: lotDecOne),
    (SY:  473; SM:  1; SD: 14; EY:  473; EM:  2; ED: 12; DayOffset: lotDecOne),
    (SY:  473; SM:  8; SD:  9; EY:  473; EM:  9; ED:  7; DayOffset: lotIncOne),
    (SY:  473; SM: 10; SD:  7; EY:  473; EM: 11; ED:  5; DayOffset: lotIncOne),
    (SY:  474; SM:  2; SD:  2; EY:  474; EM:  3; ED:  3; DayOffset: lotDecOne),
    (SY:  474; SM:  4; SD:  2; EY:  474; EM:  5; ED:  1; DayOffset: lotDecOne),
    (SY:  474; SM:  5; SD: 31; EY:  474; EM:  6; ED: 29; DayOffset: lotDecOne),
    (SY:  474; SM: 10; SD: 26; EY:  474; EM: 11; ED: 24; DayOffset: lotIncOne),
    (SY:  474; SM: 12; SD: 24; EY:  475; EM:  1; ED: 22; DayOffset: lotIncOne),
    (SY:  475; SM:  2; SD: 21; EY:  475; EM:  3; ED: 22; DayOffset: lotIncOne),
    (SY:  475; SM:  4; SD: 21; EY:  475; EM:  5; ED: 20; DayOffset: lotDecOne),
    (SY:  475; SM:  6; SD: 19; EY:  475; EM:  7; ED: 18; DayOffset: lotDecOne),
    (SY:  475; SM:  8; SD: 17; EY:  475; EM:  9; ED: 15; DayOffset: lotDecOne),
    (SY:  475; SM: 11; SD: 14; EY:  475; EM: 12; ED: 13; DayOffset: lotIncOne),
    (SY:  476; SM:  1; SD: 12; EY:  476; EM:  2; ED: 10; DayOffset: lotIncOne),
    (SY:  476; SM:  3; SD: 11; EY:  476; EM:  4; ED:  9; DayOffset: lotIncOne),
    (SY:  476; SM:  7; SD:  7; EY:  476; EM:  8; ED:  5; DayOffset: lotDecOne),
    (SY:  476; SM:  9; SD:  4; EY:  476; EM: 10; ED:  3; DayOffset: lotDecOne),
    (SY:  477; SM:  1; SD: 30; EY:  477; EM:  2; ED: 28; DayOffset: lotIncOne),
    (SY:  477; SM:  3; SD: 30; EY:  477; EM:  4; ED: 28; DayOffset: lotIncOne),
    (SY:  477; SM:  8; SD: 24; EY:  477; EM:  9; ED: 22; DayOffset: lotDecOne),
    (SY:  477; SM: 11; SD: 21; EY:  477; EM: 12; ED: 20; DayOffset: lotDecOne),
    (SY:  478; SM:  2; SD: 18; EY:  478; EM:  3; ED: 19; DayOffset: lotIncOne),
    (SY:  478; SM:  4; SD: 18; EY:  478; EM:  5; ED: 17; DayOffset: lotIncOne),
    (SY:  478; SM:  9; SD: 12; EY:  478; EM: 10; ED: 11; DayOffset: lotDecOne),
    (SY:  478; SM: 11; SD: 10; EY:  478; EM: 12; ED:  9; DayOffset: lotDecOne),
    (SY:  479; SM:  2; SD:  7; EY:  479; EM:  4; ED:  7; DayOffset: lotIncOne),
    (SY:  479; SM:  5; SD:  7; EY:  479; EM:  6; ED:  5; DayOffset: lotIncOne),
    (SY:  479; SM: 10; SD:  1; EY:  479; EM: 10; ED: 30; DayOffset: lotDecOne),
    (SY:  479; SM: 11; SD: 29; EY:  479; EM: 12; ED: 28; DayOffset: lotDecOne),
    (SY:  480; SM:  2; SD: 26; EY:  480; EM:  3; ED: 26; DayOffset: lotIncOne),
    (SY:  480; SM:  4; SD: 25; EY:  480; EM:  5; ED: 24; DayOffset: lotIncOne),
    (SY:  480; SM: 10; SD: 19; EY:  480; EM: 11; ED: 17; DayOffset: lotDecOne),
    (SY:  480; SM: 12; SD: 17; EY:  481; EM:  1; ED: 15; DayOffset: lotDecOne),
    (SY:  481; SM:  5; SD: 14; EY:  481; EM:  6; ED: 12; DayOffset: lotIncOne),
    (SY:  481; SM:  7; SD: 12; EY:  481; EM:  8; ED: 10; DayOffset: lotIncOne),
    (SY:  481; SM:  9; SD:  9; EY:  481; EM: 10; ED:  8; DayOffset: lotIncOne),
    (SY:  481; SM: 11; SD:  7; EY:  481; EM: 12; ED:  6; DayOffset: lotDecOne),
    (SY:  482; SM:  1; SD:  5; EY:  482; EM:  2; ED:  3; DayOffset: lotDecOne),
    (SY:  482; SM:  3; SD:  5; EY:  482; EM:  4; ED:  3; DayOffset: lotDecOne),
    (SY:  482; SM:  9; SD: 28; EY:  482; EM: 10; ED: 27; DayOffset: lotIncOne),
    (SY:  482; SM: 11; SD: 26; EY:  482; EM: 12; ED: 25; DayOffset: lotIncOne),
    (SY:  483; SM:  3; SD: 24; EY:  483; EM:  4; ED: 22; DayOffset: lotDecOne),
    (SY:  483; SM:  5; SD: 22; EY:  483; EM:  6; ED: 20; DayOffset: lotDecOne),
    (SY:  483; SM: 10; SD: 17; EY:  483; EM: 11; ED: 15; DayOffset: lotIncOne),
    (SY:  483; SM: 12; SD: 15; EY:  484; EM:  1; ED: 13; DayOffset: lotIncOne),
    (SY:  484; SM:  2; SD: 12; EY:  484; EM:  3; ED: 12; DayOffset: lotIncOne),
    (SY:  484; SM:  6; SD:  9; EY:  484; EM:  7; ED:  8; DayOffset: lotDecOne),
    (SY:  484; SM:  8; SD:  7; EY:  484; EM:  9; ED:  5; DayOffset: lotDecOne),
    (SY:  485; SM:  1; SD:  2; EY:  485; EM:  1; ED: 31; DayOffset: lotIncOne),
    (SY:  485; SM:  3; SD:  2; EY:  485; EM:  3; ED: 31; DayOffset: lotIncOne),
    (SY:  485; SM:  7; SD: 27; EY:  485; EM:  8; ED: 25; DayOffset: lotDecOne),
    (SY:  485; SM: 10; SD: 24; EY:  485; EM: 11; ED: 22; DayOffset: lotDecOne),
    (SY:  486; SM:  1; SD: 21; EY:  486; EM:  2; ED: 19; DayOffset: lotIncOne),
    (SY:  486; SM:  3; SD: 21; EY:  486; EM:  4; ED: 19; DayOffset: lotIncOne),
    (SY:  486; SM:  8; SD: 15; EY:  486; EM:  9; ED: 13; DayOffset: lotDecOne),
    (SY:  486; SM: 10; SD: 13; EY:  486; EM: 11; ED: 11; DayOffset: lotDecOne),
    (SY:  487; SM:  1; SD: 10; EY:  487; EM:  3; ED: 10; DayOffset: lotIncOne),
    (SY:  487; SM:  4; SD:  9; EY:  487; EM:  5; ED:  8; DayOffset: lotIncOne),
    (SY:  487; SM:  9; SD:  3; EY:  487; EM: 10; ED:  2; DayOffset: lotDecOne),
    (SY:  487; SM: 11; SD:  1; EY:  487; EM: 11; ED: 30; DayOffset: lotDecOne),
    (SY:  488; SM:  1; SD: 29; EY:  488; EM:  2; ED: 27; DayOffset: lotIncOne),
    (SY:  488; SM:  3; SD: 28; EY:  488; EM:  5; ED: 26; DayOffset: lotIncOne),
    (SY:  488; SM:  9; SD: 21; EY:  488; EM: 10; ED: 20; DayOffset: lotDecOne),
    (SY:  488; SM: 11; SD: 19; EY:  488; EM: 12; ED: 18; DayOffset: lotDecOne),
    (SY:  489; SM:  4; SD: 16; EY:  489; EM:  5; ED: 15; DayOffset: lotIncOne),
    (SY:  489; SM:  6; SD: 14; EY:  489; EM:  7; ED: 13; DayOffset: lotIncOne),
    (SY:  489; SM:  8; SD: 12; EY:  489; EM:  9; ED: 10; DayOffset: lotIncOne),
    (SY:  489; SM: 10; SD: 10; EY:  489; EM: 11; ED:  8; DayOffset: lotDecOne),
    (SY:  489; SM: 12; SD:  8; EY:  490; EM:  1; ED:  6; DayOffset: lotDecOne),
    (SY:  490; SM:  2; SD:  5; EY:  490; EM:  3; ED:  6; DayOffset: lotDecOne),
    (SY:  490; SM:  7; SD:  3; EY:  490; EM:  8; ED:  1; DayOffset: lotIncOne),
    (SY:  490; SM:  8; SD: 31; EY:  490; EM:  9; ED: 29; DayOffset: lotIncOne),
    (SY:  491; SM:  2; SD: 24; EY:  491; EM:  3; ED: 25; DayOffset: lotDecOne),
    (SY:  491; SM:  4; SD: 24; EY:  491; EM:  5; ED: 23; DayOffset: lotDecOne),
    (SY:  491; SM:  9; SD: 19; EY:  491; EM: 10; ED: 18; DayOffset: lotIncOne),
    (SY:  491; SM: 11; SD: 17; EY:  491; EM: 12; ED: 16; DayOffset: lotIncOne),
    (SY:  492; SM:  5; SD: 12; EY:  492; EM:  6; ED: 10; DayOffset: lotDecOne),
    (SY:  492; SM:  7; SD: 10; EY:  492; EM:  8; ED:  8; DayOffset: lotDecOne),
    (SY:  492; SM: 12; SD:  5; EY:  493; EM:  1; ED:  3; DayOffset: lotIncOne),
    (SY:  493; SM:  2; SD:  2; EY:  493; EM:  3; ED:  3; DayOffset: lotIncOne),
    (SY:  493; SM:  6; SD: 29; EY:  493; EM:  7; ED: 28; DayOffset: lotDecOne),
    (SY:  493; SM:  9; SD: 26; EY:  493; EM: 10; ED: 25; DayOffset: lotDecOne),
    (SY:  493; SM: 12; SD: 24; EY:  494; EM:  1; ED: 22; DayOffset: lotIncOne),
    (SY:  494; SM:  2; SD: 21; EY:  494; EM:  3; ED: 22; DayOffset: lotIncOne),
    (SY:  494; SM:  7; SD: 18; EY:  494; EM:  8; ED: 16; DayOffset: lotDecOne),
    (SY:  494; SM:  9; SD: 15; EY:  494; EM: 10; ED: 14; DayOffset: lotDecOne),
    (SY:  494; SM: 12; SD: 13; EY:  495; EM:  2; ED: 10; DayOffset: lotIncOne),
    (SY:  495; SM:  3; SD: 12; EY:  495; EM:  4; ED: 10; DayOffset: lotIncOne),
    (SY:  495; SM:  8; SD:  6; EY:  495; EM:  9; ED:  4; DayOffset: lotDecOne),
    (SY:  495; SM: 10; SD:  4; EY:  495; EM: 11; ED:  2; DayOffset: lotDecOne),
    (SY:  496; SM:  1; SD:  1; EY:  496; EM:  1; ED: 30; DayOffset: lotIncOne),
    (SY:  496; SM:  2; SD: 29; EY:  496; EM:  4; ED: 28; DayOffset: lotIncOne),
    (SY:  496; SM:  8; SD: 24; EY:  496; EM:  9; ED: 22; DayOffset: lotDecOne),
    (SY:  496; SM: 10; SD: 22; EY:  496; EM: 11; ED: 20; DayOffset: lotDecOne),
    (SY:  497; SM:  3; SD: 19; EY:  497; EM:  4; ED: 17; DayOffset: lotIncOne),
    (SY:  497; SM:  5; SD: 17; EY:  497; EM:  6; ED: 15; DayOffset: lotIncOne),
    (SY:  497; SM:  7; SD: 15; EY:  497; EM:  8; ED: 13; DayOffset: lotIncOne),
    (SY:  497; SM:  9; SD: 12; EY:  497; EM: 10; ED: 11; DayOffset: lotDecOne),
    (SY:  497; SM: 11; SD: 10; EY:  497; EM: 12; ED:  9; DayOffset: lotDecOne),
    (SY:  498; SM:  1; SD:  8; EY:  498; EM:  2; ED:  6; DayOffset: lotDecOne),
    (SY:  498; SM:  4; SD:  7; EY:  498; EM:  5; ED:  6; DayOffset: lotIncOne),
    (SY:  498; SM:  6; SD:  5; EY:  498; EM:  7; ED:  4; DayOffset: lotIncOne),
    (SY:  498; SM:  8; SD:  3; EY:  498; EM:  9; ED:  1; DayOffset: lotIncOne),
    (SY:  499; SM:  1; SD: 27; EY:  499; EM:  2; ED: 25; DayOffset: lotDecOne),
    (SY:  500; SM:  4; SD: 14; EY:  500; EM:  5; ED: 13; DayOffset: lotDecOne),
    (SY:  502; SM:  6; SD: 20; EY:  502; EM:  7; ED: 19; DayOffset: lotDecOne),
    (SY:  502; SM: 11; SD: 15; EY:  502; EM: 12; ED: 14; DayOffset: lotIncOne),
    (SY:  503; SM:  2; SD: 12; EY:  503; EM:  3; ED: 13; DayOffset: lotIncOne),
    (SY:  503; SM:  7; SD:  9; EY:  503; EM:  8; ED:  7; DayOffset: lotDecOne),
    (SY:  503; SM:  9; SD:  6; EY:  503; EM: 10; ED:  5; DayOffset: lotDecOne),
    (SY:  503; SM: 12; SD:  4; EY:  504; EM:  1; ED:  2; DayOffset: lotIncOne),
    (SY:  504; SM:  2; SD:  1; EY:  504; EM:  3; ED: 31; DayOffset: lotIncOne),
    (SY:  504; SM:  7; SD: 27; EY:  504; EM:  8; ED: 25; DayOffset: lotDecOne),
    (SY:  504; SM:  9; SD: 24; EY:  504; EM: 10; ED: 23; DayOffset: lotDecOne),
    (SY:  505; SM:  2; SD: 19; EY:  505; EM:  3; ED: 20; DayOffset: lotIncOne),
    (SY:  505; SM:  4; SD: 19; EY:  505; EM:  5; ED: 18; DayOffset: lotIncOne),
    (SY:  505; SM:  6; SD: 17; EY:  505; EM:  7; ED: 16; DayOffset: lotIncOne),
    (SY:  505; SM:  8; SD: 15; EY:  505; EM:  9; ED: 13; DayOffset: lotDecOne),
    (SY:  505; SM: 10; SD: 13; EY:  505; EM: 11; ED: 11; DayOffset: lotDecOne),
    (SY:  505; SM: 12; SD: 11; EY:  506; EM:  1; ED:  9; DayOffset: lotDecOne),
    (SY:  506; SM:  3; SD: 10; EY:  506; EM:  4; ED:  8; DayOffset: lotIncOne),
    (SY:  506; SM:  5; SD:  8; EY:  506; EM:  6; ED:  6; DayOffset: lotIncOne),
    (SY:  506; SM:  7; SD:  6; EY:  506; EM:  8; ED:  4; DayOffset: lotIncOne),
    (SY:  506; SM: 12; SD: 30; EY:  507; EM:  1; ED: 28; DayOffset: lotDecOne),
    (SY:  507; SM:  5; SD: 27; EY:  507; EM:  6; ED: 25; DayOffset: lotIncOne),
    (SY:  507; SM:  7; SD: 25; EY:  507; EM:  8; ED: 23; DayOffset: lotIncOne),
    (SY:  508; SM:  3; SD: 17; EY:  508; EM:  4; ED: 15; DayOffset: lotDecOne),
    (SY:  510; SM:  5; SD: 23; EY:  510; EM:  6; ED: 21; DayOffset: lotDecOne),
    (SY:  510; SM: 10; SD: 18; EY:  510; EM: 11; ED: 16; DayOffset: lotIncOne),
    (SY:  511; SM:  6; SD: 11; EY:  511; EM:  7; ED: 10; DayOffset: lotDecOne),
    (SY:  511; SM:  8; SD:  9; EY:  511; EM:  9; ED:  7; DayOffset: lotDecOne),
    (SY:  511; SM: 11; SD:  6; EY:  511; EM: 12; ED:  5; DayOffset: lotIncOne),
    (SY:  512; SM:  1; SD:  4; EY:  512; EM:  2; ED:  2; DayOffset: lotIncOne),
    (SY:  512; SM:  6; SD: 29; EY:  512; EM:  7; ED: 28; DayOffset: lotDecOne),
    (SY:  512; SM:  8; SD: 27; EY:  512; EM:  9; ED: 25; DayOffset: lotDecOne),
    (SY:  513; SM:  1; SD: 22; EY:  513; EM:  2; ED: 20; DayOffset: lotIncOne),
    (SY:  513; SM:  3; SD: 22; EY:  513; EM:  4; ED: 20; DayOffset: lotIncOne),
    (SY:  513; SM:  5; SD: 20; EY:  513; EM:  6; ED: 18; DayOffset: lotIncOne),
    (SY:  513; SM:  7; SD: 18; EY:  513; EM:  8; ED: 16; DayOffset: lotDecOne),
    (SY:  513; SM:  9; SD: 15; EY:  513; EM: 10; ED: 14; DayOffset: lotDecOne),
    (SY:  513; SM: 11; SD: 13; EY:  513; EM: 12; ED: 12; DayOffset: lotDecOne),
    (SY:  514; SM:  2; SD: 10; EY:  514; EM:  3; ED: 11; DayOffset: lotIncOne),
    (SY:  514; SM:  4; SD: 10; EY:  514; EM:  5; ED:  9; DayOffset: lotIncOne),
    (SY:  514; SM:  6; SD:  8; EY:  514; EM:  7; ED:  7; DayOffset: lotIncOne),
    (SY:  514; SM: 12; SD:  2; EY:  514; EM: 12; ED: 31; DayOffset: lotDecOne),
    (SY:  515; SM:  4; SD: 29; EY:  515; EM:  5; ED: 28; DayOffset: lotIncOne),
    (SY:  515; SM:  6; SD: 27; EY:  515; EM:  7; ED: 26; DayOffset: lotIncOne),
    (SY:  517; SM:  5; SD:  6; EY:  517; EM:  6; ED:  4; DayOffset: lotIncOne),
    (SY:  518; SM:  9; SD: 20; EY:  518; EM: 10; ED: 19; DayOffset: lotIncOne),
    (SY:  519; SM:  5; SD: 14; EY:  519; EM:  6; ED: 12; DayOffset: lotDecOne),
    (SY:  519; SM:  7; SD: 12; EY:  519; EM:  8; ED: 10; DayOffset: lotDecOne),
    (SY:  519; SM: 10; SD:  9; EY:  519; EM: 11; ED:  7; DayOffset: lotIncOne),
    (SY:  519; SM: 12; SD:  7; EY:  520; EM:  1; ED:  5; DayOffset: lotIncOne),
    (SY:  520; SM:  2; SD:  4; EY:  520; EM:  3; ED:  4; DayOffset: lotIncOne),
    (SY:  520; SM:  6; SD:  1; EY:  520; EM:  6; ED: 30; DayOffset: lotDecOne),
    (SY:  520; SM:  7; SD: 30; EY:  520; EM:  8; ED: 28; DayOffset: lotDecOne),
    (SY:  520; SM: 12; SD: 25; EY:  521; EM:  1; ED: 23; DayOffset: lotIncOne),
    (SY:  521; SM:  2; SD: 22; EY:  521; EM:  3; ED: 23; DayOffset: lotIncOne),
    (SY:  521; SM:  4; SD: 22; EY:  521; EM:  5; ED: 21; DayOffset: lotIncOne),
    (SY:  521; SM:  6; SD: 20; EY:  521; EM:  7; ED: 19; DayOffset: lotDecOne),
    (SY:  521; SM:  8; SD: 18; EY:  521; EM:  9; ED: 16; DayOffset: lotDecOne),
    (SY:  521; SM: 10; SD: 16; EY:  521; EM: 11; ED: 14; DayOffset: lotDecOne),
    (SY:  522; SM:  1; SD: 13; EY:  522; EM:  2; ED: 11; DayOffset: lotIncOne),
    (SY:  522; SM:  3; SD: 13; EY:  522; EM:  4; ED: 11; DayOffset: lotIncOne),
    (SY:  522; SM:  5; SD: 11; EY:  522; EM:  6; ED:  9; DayOffset: lotIncOne),
    (SY:  522; SM: 10; SD:  5; EY:  522; EM: 12; ED:  3; DayOffset: lotDecOne),
    (SY:  523; SM:  4; SD:  1; EY:  523; EM:  4; ED: 30; DayOffset: lotIncOne),
    (SY:  523; SM:  5; SD: 30; EY:  523; EM:  6; ED: 28; DayOffset: lotIncOne),
    (SY:  523; SM: 10; SD: 24; EY:  523; EM: 11; ED: 22; DayOffset: lotDecOne),
    (SY:  524; SM:  4; SD: 19; EY:  524; EM:  5; ED: 18; DayOffset: lotIncOne),
    (SY:  524; SM:  6; SD: 17; EY:  524; EM:  7; ED: 16; DayOffset: lotIncOne),
    (SY:  524; SM: 11; SD: 11; EY:  524; EM: 12; ED: 10; DayOffset: lotDecOne),
    (SY:  525; SM:  4; SD:  8; EY:  525; EM:  5; ED:  7; DayOffset: lotIncOne),
    (SY:  526; SM:  6; SD: 25; EY:  526; EM:  7; ED: 24; DayOffset: lotIncOne),
    (SY:  526; SM:  8; SD: 23; EY:  526; EM:  9; ED: 21; DayOffset: lotIncOne),
    (SY:  527; SM:  2; SD: 16; EY:  527; EM:  3; ED: 17; DayOffset: lotDecOne),
    (SY:  527; SM:  4; SD: 16; EY:  527; EM:  5; ED: 15; DayOffset: lotDecOne),
    (SY:  527; SM:  9; SD: 11; EY:  527; EM: 10; ED: 10; DayOffset: lotIncOne),
    (SY:  527; SM: 11; SD:  9; EY:  527; EM: 12; ED:  8; DayOffset: lotIncOne),
    (SY:  528; SM:  1; SD:  7; EY:  528; EM:  2; ED:  5; DayOffset: lotIncOne),
    (SY:  528; SM:  5; SD:  4; EY:  528; EM:  6; ED:  2; DayOffset: lotDecOne),
    (SY:  528; SM:  7; SD:  2; EY:  528; EM:  7; ED: 31; DayOffset: lotDecOne),
    (SY:  528; SM: 11; SD: 27; EY:  528; EM: 12; ED: 26; DayOffset: lotIncOne),
    (SY:  529; SM:  1; SD: 25; EY:  529; EM:  2; ED: 23; DayOffset: lotIncOne),
    (SY:  529; SM:  3; SD: 25; EY:  529; EM:  4; ED: 23; DayOffset: lotIncOne),
    (SY:  529; SM:  5; SD: 23; EY:  529; EM:  6; ED: 21; DayOffset: lotDecOne),
    (SY:  529; SM:  7; SD: 21; EY:  529; EM:  8; ED: 19; DayOffset: lotDecOne),
    (SY:  529; SM:  9; SD: 18; EY:  529; EM: 10; ED: 17; DayOffset: lotDecOne),
    (SY:  529; SM: 12; SD: 16; EY:  530; EM:  1; ED: 14; DayOffset: lotIncOne),
    (SY:  530; SM:  2; SD: 13; EY:  530; EM:  3; ED: 14; DayOffset: lotIncOne),
    (SY:  530; SM:  4; SD: 13; EY:  530; EM:  5; ED: 12; DayOffset: lotIncOne),
    (SY:  530; SM:  9; SD:  7; EY:  530; EM: 11; ED:  5; DayOffset: lotDecOne),
    (SY:  531; SM:  3; SD:  4; EY:  531; EM:  4; ED:  2; DayOffset: lotIncOne),
    (SY:  531; SM:  5; SD:  2; EY:  531; EM:  5; ED: 31; DayOffset: lotIncOne),
    (SY:  531; SM:  9; SD: 26; EY:  531; EM: 10; ED: 25; DayOffset: lotDecOne),
    (SY:  532; SM:  3; SD: 22; EY:  532; EM:  4; ED: 20; DayOffset: lotIncOne),
    (SY:  532; SM:  5; SD: 20; EY:  532; EM:  6; ED: 18; DayOffset: lotIncOne),
    (SY:  532; SM: 10; SD: 14; EY:  532; EM: 11; ED: 12; DayOffset: lotDecOne),
    (SY:  533; SM:  3; SD: 11; EY:  533; EM:  4; ED:  9; DayOffset: lotIncOne),
    (SY:  533; SM: 11; SD:  2; EY:  533; EM: 12; ED:  1; DayOffset: lotDecOne),
    (SY:  533; SM: 12; SD: 31; EY:  534; EM:  1; ED: 29; DayOffset: lotDecOne),
    (SY:  534; SM:  5; SD: 28; EY:  534; EM:  6; ED: 26; DayOffset: lotIncOne),
    (SY:  534; SM:  7; SD: 26; EY:  534; EM:  8; ED: 24; DayOffset: lotIncOne),
    (SY:  534; SM: 11; SD: 21; EY:  534; EM: 12; ED: 20; DayOffset: lotDecOne),
    (SY:  535; SM:  1; SD: 19; EY:  535; EM:  2; ED: 17; DayOffset: lotDecOne),
    (SY:  535; SM:  8; SD: 14; EY:  535; EM:  9; ED: 12; DayOffset: lotIncOne),
    (SY:  535; SM: 10; SD: 12; EY:  535; EM: 11; ED: 10; DayOffset: lotIncOne),
    (SY:  535; SM: 12; SD: 10; EY:  536; EM:  1; ED:  8; DayOffset: lotIncOne),
    (SY:  536; SM:  2; SD:  7; EY:  536; EM:  3; ED:  7; DayOffset: lotDecOne),
    (SY:  536; SM:  4; SD:  6; EY:  536; EM:  5; ED:  5; DayOffset: lotDecOne),
    (SY:  536; SM:  6; SD:  4; EY:  536; EM:  7; ED:  3; DayOffset: lotDecOne),
    (SY:  536; SM: 10; SD: 30; EY:  536; EM: 11; ED: 28; DayOffset: lotIncOne),
    (SY:  536; SM: 12; SD: 28; EY:  537; EM:  1; ED: 26; DayOffset: lotIncOne),
    (SY:  537; SM:  2; SD: 25; EY:  537; EM:  3; ED: 26; DayOffset: lotIncOne),
    (SY:  537; SM:  4; SD: 25; EY:  537; EM:  5; ED: 24; DayOffset: lotDecOne),
    (SY:  537; SM:  6; SD: 23; EY:  537; EM:  7; ED: 22; DayOffset: lotDecOne),
    (SY:  537; SM:  8; SD: 21; EY:  537; EM:  9; ED: 19; DayOffset: lotDecOne),
    (SY:  537; SM: 11; SD: 18; EY:  537; EM: 12; ED: 17; DayOffset: lotIncOne),
    (SY:  538; SM:  1; SD: 16; EY:  538; EM:  2; ED: 14; DayOffset: lotIncOne),
    (SY:  538; SM:  3; SD: 16; EY:  538; EM:  4; ED: 14; DayOffset: lotIncOne),
    (SY:  538; SM:  8; SD: 10; EY:  538; EM: 10; ED:  8; DayOffset: lotDecOne),
    (SY:  539; SM:  2; SD:  4; EY:  539; EM:  3; ED:  5; DayOffset: lotIncOne),
    (SY:  539; SM:  4; SD:  4; EY:  539; EM:  5; ED:  3; DayOffset: lotIncOne),
    (SY:  539; SM:  7; SD:  1; EY:  539; EM:  7; ED: 30; DayOffset: lotDecOne),
    (SY:  539; SM:  8; SD: 29; EY:  539; EM:  9; ED: 27; DayOffset: lotDecOne),
    (SY:  540; SM:  2; SD: 23; EY:  540; EM:  3; ED: 23; DayOffset: lotIncOne),
    (SY:  540; SM:  4; SD: 22; EY:  540; EM:  5; ED: 21; DayOffset: lotIncOne),
    (SY:  540; SM:  9; SD: 16; EY:  540; EM: 10; ED: 15; DayOffset: lotDecOne),
    (SY:  540; SM: 11; SD: 14; EY:  540; EM: 12; ED: 13; DayOffset: lotDecOne),
    (SY:  541; SM:  2; SD: 11; EY:  541; EM:  3; ED: 12; DayOffset: lotIncOne),
    (SY:  541; SM:  5; SD: 11; EY:  541; EM:  6; ED:  9; DayOffset: lotIncOne),
    (SY:  541; SM: 10; SD:  5; EY:  541; EM: 11; ED:  3; DayOffset: lotDecOne),
    (SY:  541; SM: 12; SD:  3; EY:  542; EM:  1; ED:  1; DayOffset: lotDecOne),
    (SY:  542; SM:  3; SD:  2; EY:  542; EM:  3; ED: 31; DayOffset: lotIncOne),
    (SY:  542; SM:  4; SD: 30; EY:  542; EM:  5; ED: 29; DayOffset: lotIncOne),
    (SY:  542; SM:  6; SD: 28; EY:  542; EM:  7; ED: 27; DayOffset: lotIncOne),
    (SY:  542; SM: 10; SD: 24; EY:  542; EM: 11; ED: 22; DayOffset: lotDecOne),
    (SY:  542; SM: 12; SD: 22; EY:  543; EM:  1; ED: 20; DayOffset: lotDecOne),
    (SY:  543; SM:  5; SD: 19; EY:  543; EM:  6; ED: 17; DayOffset: lotIncOne),
    (SY:  543; SM:  7; SD: 17; EY:  543; EM:  8; ED: 15; DayOffset: lotIncOne),
    (SY:  543; SM:  9; SD: 14; EY:  543; EM: 10; ED: 13; DayOffset: lotIncOne),
    (SY:  544; SM:  1; SD: 10; EY:  544; EM:  2; ED:  8; DayOffset: lotDecOne),
    (SY:  544; SM:  3; SD:  9; EY:  544; EM:  4; ED:  7; DayOffset: lotDecOne),
    (SY:  544; SM: 10; SD:  2; EY:  544; EM: 10; ED: 31; DayOffset: lotIncOne),
    (SY:  544; SM: 11; SD: 30; EY:  544; EM: 12; ED: 29; DayOffset: lotIncOne),
    (SY:  545; SM:  3; SD: 28; EY:  545; EM:  4; ED: 26; DayOffset: lotDecOne),
    (SY:  545; SM:  5; SD: 26; EY:  545; EM:  6; ED: 24; DayOffset: lotDecOne),
    (SY:  545; SM: 10; SD: 21; EY:  545; EM: 11; ED: 19; DayOffset: lotIncOne),
    (SY:  545; SM: 12; SD: 19; EY:  546; EM:  1; ED: 17; DayOffset: lotIncOne),
    (SY:  546; SM:  8; SD: 12; EY:  546; EM:  9; ED: 10; DayOffset: lotDecOne),
    (SY:  547; SM:  1; SD:  7; EY:  547; EM:  2; ED:  5; DayOffset: lotIncOne),
    (SY:  547; SM:  3; SD:  7; EY:  547; EM:  4; ED:  5; DayOffset: lotIncOne),
    (SY:  547; SM:  8; SD:  1; EY:  547; EM:  8; ED: 30; DayOffset: lotDecOne),
    (SY:  548; SM:  1; SD: 26; EY:  548; EM:  2; ED: 24; DayOffset: lotIncOne),
    (SY:  548; SM:  3; SD: 25; EY:  548; EM:  4; ED: 23; DayOffset: lotIncOne),
    (SY:  548; SM:  6; SD: 21; EY:  548; EM:  7; ED: 20; DayOffset: lotDecOne),
    (SY:  548; SM:  8; SD: 19; EY:  548; EM:  9; ED: 17; DayOffset: lotDecOne),
    (SY:  548; SM: 10; SD: 17; EY:  548; EM: 11; ED: 15; DayOffset: lotDecOne),
    (SY:  549; SM:  1; SD: 14; EY:  549; EM:  3; ED: 14; DayOffset: lotIncOne),
    (SY:  549; SM:  4; SD: 13; EY:  549; EM:  5; ED: 12; DayOffset: lotIncOne),
    (SY:  549; SM:  9; SD:  7; EY:  549; EM: 10; ED:  6; DayOffset: lotDecOne),
    (SY:  549; SM: 11; SD:  5; EY:  549; EM: 12; ED:  4; DayOffset: lotDecOne),
    (SY:  550; SM:  2; SD:  2; EY:  550; EM:  3; ED:  3; DayOffset: lotIncOne),
    (SY:  550; SM:  4; SD:  2; EY:  550; EM:  5; ED:  1; DayOffset: lotIncOne),
    (SY:  550; SM:  5; SD: 31; EY:  550; EM:  6; ED: 29; DayOffset: lotIncOne),
    (SY:  550; SM:  9; SD: 26; EY:  550; EM: 10; ED: 25; DayOffset: lotDecOne),
    (SY:  550; SM: 11; SD: 24; EY:  550; EM: 12; ED: 23; DayOffset: lotDecOne),
    (SY:  551; SM:  4; SD: 21; EY:  551; EM:  5; ED: 20; DayOffset: lotIncOne),
    (SY:  551; SM:  6; SD: 19; EY:  551; EM:  7; ED: 18; DayOffset: lotIncOne),
    (SY:  551; SM:  8; SD: 17; EY:  551; EM:  9; ED: 15; DayOffset: lotIncOne),
    (SY:  551; SM: 12; SD: 13; EY:  552; EM:  1; ED: 11; DayOffset: lotDecOne),
    (SY:  552; SM:  2; SD: 10; EY:  552; EM:  3; ED: 10; DayOffset: lotDecOne),
    (SY:  552; SM:  7; SD:  7; EY:  552; EM:  8; ED:  5; DayOffset: lotIncOne),
    (SY:  552; SM:  9; SD:  4; EY:  552; EM: 10; ED:  3; DayOffset: lotIncOne),
    (SY:  553; SM:  2; SD: 28; EY:  553; EM:  3; ED: 29; DayOffset: lotDecOne),
    (SY:  553; SM:  4; SD: 28; EY:  553; EM:  5; ED: 27; DayOffset: lotDecOne),
    (SY:  554; SM:  7; SD: 15; EY:  554; EM:  8; ED: 13; DayOffset: lotDecOne),
    (SY:  554; SM: 12; SD: 10; EY:  555; EM:  1; ED:  8; DayOffset: lotIncOne),
    (SY:  555; SM:  7; SD:  4; EY:  555; EM:  8; ED:  2; DayOffset: lotDecOne),
    (SY:  555; SM: 12; SD: 29; EY:  556; EM:  1; ED: 27; DayOffset: lotIncOne),
    (SY:  556; SM:  2; SD: 26; EY:  556; EM:  3; ED: 26; DayOffset: lotIncOne),
    (SY:  556; SM:  7; SD: 22; EY:  556; EM:  8; ED: 20; DayOffset: lotDecOne),
    (SY:  556; SM:  9; SD: 19; EY:  556; EM: 10; ED: 18; DayOffset: lotDecOne),
    (SY:  556; SM: 12; SD: 17; EY:  557; EM:  1; ED: 15; DayOffset: lotIncOne),
    (SY:  557; SM:  3; SD: 16; EY:  557; EM:  4; ED: 14; DayOffset: lotIncOne),
    (SY:  557; SM:  6; SD: 12; EY:  557; EM:  7; ED: 11; DayOffset: lotDecOne),
    (SY:  557; SM:  8; SD: 10; EY:  557; EM:  9; ED:  8; DayOffset: lotDecOne),
    (SY:  557; SM: 10; SD:  8; EY:  557; EM: 11; ED:  6; DayOffset: lotDecOne),
    (SY:  558; SM:  1; SD:  5; EY:  558; EM:  2; ED:  3; DayOffset: lotIncOne),
    (SY:  558; SM:  3; SD:  5; EY:  558; EM:  6; ED:  1; DayOffset: lotIncOne),
    (SY:  558; SM:  8; SD: 29; EY:  558; EM:  9; ED: 27; DayOffset: lotDecOne),
    (SY:  558; SM: 10; SD: 27; EY:  558; EM: 11; ED: 25; DayOffset: lotDecOne),
    (SY:  559; SM:  3; SD: 24; EY:  559; EM:  4; ED: 22; DayOffset: lotIncOne),
    (SY:  559; SM:  5; SD: 22; EY:  559; EM:  6; ED: 20; DayOffset: lotIncOne),
    (SY:  559; SM:  7; SD: 20; EY:  559; EM:  8; ED: 18; DayOffset: lotIncOne),
    (SY:  559; SM: 11; SD: 15; EY:  559; EM: 12; ED: 14; DayOffset: lotDecOne),
    (SY:  560; SM:  1; SD: 13; EY:  560; EM:  2; ED: 11; DayOffset: lotDecOne),
    (SY:  560; SM:  4; SD: 11; EY:  560; EM:  5; ED: 10; DayOffset: lotIncOne),
    (SY:  560; SM:  6; SD:  9; EY:  560; EM:  7; ED:  8; DayOffset: lotIncOne),
    (SY:  560; SM:  8; SD:  7; EY:  560; EM:  9; ED:  5; DayOffset: lotIncOne),
    (SY:  561; SM:  1; SD: 31; EY:  561; EM:  3; ED:  1; DayOffset: lotDecOne),
    (SY:  564; SM:  6; SD: 24; EY:  564; EM:  7; ED: 23; DayOffset: lotDecOne),
    (SY:  564; SM:  8; SD: 22; EY:  564; EM:  9; ED: 20; DayOffset: lotDecOne),
    (SY:  564; SM: 11; SD: 19; EY:  564; EM: 12; ED: 18; DayOffset: lotIncOne),
    (SY:  565; SM:  7; SD: 13; EY:  565; EM:  8; ED: 11; DayOffset: lotDecOne),
    (SY:  565; SM:  9; SD: 10; EY:  565; EM: 10; ED:  9; DayOffset: lotDecOne),
    (SY:  565; SM: 12; SD:  8; EY:  566; EM:  1; ED:  6; DayOffset: lotIncOne),
    (SY:  566; SM:  2; SD:  5; EY:  566; EM:  5; ED:  4; DayOffset: lotIncOne),
    (SY:  566; SM:  8; SD:  1; EY:  566; EM:  8; ED: 30; DayOffset: lotDecOne),
    (SY:  566; SM:  9; SD: 29; EY:  566; EM: 10; ED: 28; DayOffset: lotDecOne),
    (SY:  567; SM:  2; SD: 24; EY:  567; EM:  3; ED: 25; DayOffset: lotIncOne),
    (SY:  567; SM:  4; SD: 24; EY:  567; EM:  5; ED: 23; DayOffset: lotIncOne),
    (SY:  567; SM:  6; SD: 22; EY:  567; EM:  7; ED: 21; DayOffset: lotIncOne),
    (SY:  567; SM: 10; SD: 18; EY:  567; EM: 11; ED: 16; DayOffset: lotDecOne),
    (SY:  567; SM: 12; SD: 16; EY:  568; EM:  1; ED: 14; DayOffset: lotDecOne),
    (SY:  568; SM:  3; SD: 14; EY:  568; EM:  4; ED: 12; DayOffset: lotIncOne),
    (SY:  568; SM:  5; SD: 12; EY:  568; EM:  6; ED: 10; DayOffset: lotIncOne),
    (SY:  568; SM:  7; SD: 10; EY:  568; EM:  8; ED:  8; DayOffset: lotIncOne),
    (SY:  569; SM:  1; SD:  3; EY:  569; EM:  2; ED:  1; DayOffset: lotDecOne),
    (SY:  569; SM:  5; SD: 31; EY:  569; EM:  6; ED: 29; DayOffset: lotIncOne),
    (SY:  571; SM:  8; SD:  6; EY:  571; EM:  9; ED:  4; DayOffset: lotIncOne),
    (SY:  572; SM:  5; SD: 27; EY:  572; EM:  6; ED: 25; DayOffset: lotDecOne),
    (SY:  572; SM: 10; SD: 22; EY:  572; EM: 11; ED: 20; DayOffset: lotIncOne),
    (SY:  573; SM:  6; SD: 15; EY:  573; EM:  7; ED: 14; DayOffset: lotDecOne),
    (SY:  573; SM:  8; SD: 13; EY:  573; EM:  9; ED: 11; DayOffset: lotDecOne),
    (SY:  573; SM: 11; SD: 10; EY:  573; EM: 12; ED:  9; DayOffset: lotIncOne),
    (SY:  574; SM:  1; SD:  8; EY:  574; EM:  2; ED:  6; DayOffset: lotIncOne),
    (SY:  574; SM:  3; SD:  8; EY:  574; EM:  4; ED:  6; DayOffset: lotIncOne),
    (SY:  574; SM:  7; SD:  4; EY:  574; EM:  8; ED:  2; DayOffset: lotDecOne),
    (SY:  574; SM:  9; SD:  1; EY:  574; EM:  9; ED: 30; DayOffset: lotDecOne),
    (SY:  575; SM:  1; SD: 27; EY:  575; EM:  2; ED: 25; DayOffset: lotIncOne),
    (SY:  575; SM:  3; SD: 27; EY:  575; EM:  4; ED: 25; DayOffset: lotIncOne),
    (SY:  575; SM:  5; SD: 25; EY:  575; EM:  6; ED: 23; DayOffset: lotIncOne),
    (SY:  575; SM:  9; SD: 20; EY:  575; EM: 10; ED: 19; DayOffset: lotDecOne),
    (SY:  575; SM: 11; SD: 18; EY:  575; EM: 12; ED: 17; DayOffset: lotDecOne),
    (SY:  576; SM:  2; SD: 15; EY:  576; EM:  3; ED: 15; DayOffset: lotIncOne),
    (SY:  576; SM:  4; SD: 14; EY:  576; EM:  5; ED: 13; DayOffset: lotIncOne),
    (SY:  576; SM:  6; SD: 12; EY:  576; EM:  7; ED: 11; DayOffset: lotIncOne),
    (SY:  576; SM: 11; SD:  6; EY:  577; EM:  1; ED:  4; DayOffset: lotDecOne),
    (SY:  577; SM:  5; SD:  3; EY:  577; EM:  6; ED:  1; DayOffset: lotIncOne),
    (SY:  577; SM:  7; SD:  1; EY:  577; EM:  7; ED: 30; DayOffset: lotIncOne),
    (SY:  577; SM: 11; SD: 25; EY:  577; EM: 12; ED: 24; DayOffset: lotDecOne),
    (SY:  579; SM:  7; SD:  9; EY:  579; EM:  8; ED:  7; DayOffset: lotIncOne),
    (SY:  580; SM:  9; SD: 24; EY:  580; EM: 10; ED: 23; DayOffset: lotIncOne),
    (SY:  581; SM:  5; SD: 18; EY:  581; EM:  6; ED: 16; DayOffset: lotDecOne),
    (SY:  581; SM:  7; SD: 16; EY:  581; EM:  8; ED: 14; DayOffset: lotDecOne),
    (SY:  581; SM: 10; SD: 13; EY:  581; EM: 11; ED: 11; DayOffset: lotIncOne),
    (SY:  581; SM: 12; SD: 11; EY:  582; EM:  1; ED:  9; DayOffset: lotIncOne),
    (SY:  582; SM:  2; SD:  8; EY:  582; EM:  3; ED:  9; DayOffset: lotIncOne),
    (SY:  582; SM:  6; SD:  6; EY:  582; EM:  7; ED:  5; DayOffset: lotDecOne),
    (SY:  582; SM:  8; SD:  4; EY:  582; EM:  9; ED:  2; DayOffset: lotDecOne),
    (SY:  582; SM: 12; SD: 30; EY:  583; EM:  1; ED: 28; DayOffset: lotIncOne),
    (SY:  583; SM:  2; SD: 27; EY:  583; EM:  3; ED: 28; DayOffset: lotIncOne),
    (SY:  583; SM:  4; SD: 27; EY:  583; EM:  5; ED: 26; DayOffset: lotIncOne),
    (SY:  583; SM:  7; SD: 24; EY:  583; EM:  9; ED: 21; DayOffset: lotDecOne),
    (SY:  583; SM: 10; SD: 21; EY:  583; EM: 11; ED: 19; DayOffset: lotDecOne),
    (SY:  584; SM:  1; SD: 18; EY:  584; EM:  2; ED: 16; DayOffset: lotIncOne),
    (SY:  584; SM:  3; SD: 17; EY:  584; EM:  4; ED: 15; DayOffset: lotIncOne),
    (SY:  584; SM:  5; SD: 15; EY:  584; EM:  6; ED: 13; DayOffset: lotIncOne),
    (SY:  584; SM:  8; SD: 11; EY:  584; EM:  9; ED:  9; DayOffset: lotDecOne),
    (SY:  584; SM: 10; SD:  9; EY:  584; EM: 12; ED:  7; DayOffset: lotDecOne),
    (SY:  585; SM:  4; SD:  5; EY:  585; EM:  5; ED:  4; DayOffset: lotIncOne),
    (SY:  585; SM:  6; SD:  3; EY:  585; EM:  7; ED:  2; DayOffset: lotIncOne),
    (SY:  585; SM:  8; SD: 30; EY:  585; EM:  9; ED: 28; DayOffset: lotDecOne),
    (SY:  585; SM: 10; SD: 28; EY:  585; EM: 11; ED: 26; DayOffset: lotDecOne),
    (SY:  586; SM:  4; SD: 24; EY:  586; EM:  5; ED: 23; DayOffset: lotIncOne),
    (SY:  586; SM: 11; SD: 16; EY:  586; EM: 12; ED: 15; DayOffset: lotDecOne),
    (SY:  587; SM:  4; SD: 13; EY:  587; EM:  5; ED: 12; DayOffset: lotIncOne),
    (SY:  587; SM:  6; SD: 11; EY:  587; EM:  7; ED: 10; DayOffset: lotIncOne),
    (SY:  587; SM: 12; SD:  5; EY:  588; EM:  1; ED:  3; DayOffset: lotDecOne),
    (SY:  588; SM:  2; SD:  2; EY:  588; EM:  3; ED:  2; DayOffset: lotDecOne),
    (SY:  588; SM:  6; SD: 29; EY:  588; EM:  7; ED: 28; DayOffset: lotIncOne),
    (SY:  588; SM:  8; SD: 27; EY:  588; EM:  9; ED: 25; DayOffset: lotIncOne),
    (SY:  589; SM:  2; SD: 20; EY:  589; EM:  3; ED: 21; DayOffset: lotDecOne),
    (SY:  589; SM:  4; SD: 20; EY:  589; EM:  5; ED: 19; DayOffset: lotDecOne),
    (SY:  589; SM:  9; SD: 15; EY:  589; EM: 10; ED: 14; DayOffset: lotIncOne),
    (SY:  589; SM: 11; SD: 13; EY:  589; EM: 12; ED: 12; DayOffset: lotIncOne),
    (SY:  590; SM:  1; SD: 11; EY:  590; EM:  2; ED:  9; DayOffset: lotIncOne),
    (SY:  590; SM:  3; SD: 11; EY:  590; EM:  4; ED:  9; DayOffset: lotDecOne),
    (SY:  590; SM:  5; SD:  9; EY:  590; EM:  6; ED:  7; DayOffset: lotDecOne),
    (SY:  590; SM:  7; SD:  7; EY:  590; EM:  8; ED:  5; DayOffset: lotDecOne),
    (SY:  591; SM:  5; SD: 28; EY:  591; EM:  6; ED: 26; DayOffset: lotDecOne),
    (SY:  591; SM:  7; SD: 26; EY:  591; EM:  8; ED: 24; DayOffset: lotDecOne),
    (SY:  591; SM:  9; SD: 23; EY:  591; EM: 10; ED: 22; DayOffset: lotDecOne),
    (SY:  591; SM: 12; SD: 21; EY:  592; EM:  1; ED: 19; DayOffset: lotIncOne),
    (SY:  592; SM:  2; SD: 18; EY:  592; EM:  3; ED: 18; DayOffset: lotIncOne),
    (SY:  592; SM:  6; SD: 15; EY:  592; EM: 11; ED:  9; DayOffset: lotDecOne),
    (SY:  593; SM:  3; SD:  8; EY:  593; EM:  4; ED:  6; DayOffset: lotIncOne),
    (SY:  593; SM:  5; SD:  6; EY:  593; EM:  6; ED:  4; DayOffset: lotIncOne),
    (SY:  593; SM:  7; SD:  4; EY:  593; EM: 11; ED: 28; DayOffset: lotDecOne),
    (SY:  593; SM: 12; SD: 28; EY:  594; EM:  1; ED: 26; DayOffset: lotDecOne),
    (SY:  594; SM:  3; SD: 27; EY:  594; EM:  4; ED: 25; DayOffset: lotIncOne),
    (SY:  594; SM:  5; SD: 25; EY:  594; EM:  6; ED: 23; DayOffset: lotIncOne),
    (SY:  594; SM:  8; SD: 21; EY:  594; EM:  9; ED: 19; DayOffset: lotDecOne),
    (SY:  594; SM: 10; SD: 19; EY:  595; EM:  2; ED: 14; DayOffset: lotDecOne),
    (SY:  595; SM: 11; SD:  7; EY:  595; EM: 12; ED:  6; DayOffset: lotDecOne),
    (SY:  596; SM:  1; SD:  5; EY:  596; EM:  2; ED:  3; DayOffset: lotDecOne),
    (SY:  596; SM:  4; SD:  3; EY:  596; EM:  5; ED:  2; DayOffset: lotDecOne),
    (SY:  596; SM: 11; SD: 25; EY:  596; EM: 12; ED: 24; DayOffset: lotDecOne),
    (SY:  597; SM:  1; SD: 23; EY:  597; EM:  2; ED: 21; DayOffset: lotDecOne),
    (SY:  597; SM:  3; SD: 23; EY:  597; EM:  4; ED: 21; DayOffset: lotDecOne),
    (SY:  597; SM:  8; SD: 18; EY:  597; EM:  9; ED: 16; DayOffset: lotIncOne),
    (SY:  597; SM: 10; SD: 16; EY:  597; EM: 11; ED: 14; DayOffset: lotIncOne),
    (SY:  598; SM:  2; SD: 11; EY:  598; EM:  3; ED: 12; DayOffset: lotDecOne),
    (SY:  598; SM:  4; SD: 11; EY:  598; EM:  5; ED: 10; DayOffset: lotDecOne),
    (SY:  598; SM:  6; SD:  9; EY:  598; EM:  7; ED:  8; DayOffset: lotDecOne),
    (SY:  598; SM: 11; SD:  4; EY:  598; EM: 12; ED:  3; DayOffset: lotIncOne),
    (SY:  599; SM:  1; SD:  2; EY:  599; EM:  1; ED: 31; DayOffset: lotIncOne),
    (SY:  599; SM:  4; SD: 30; EY:  599; EM:  5; ED: 29; DayOffset: lotDecOne),
    (SY:  599; SM:  6; SD: 28; EY:  599; EM:  7; ED: 27; DayOffset: lotDecOne),
    (SY:  599; SM:  8; SD: 26; EY:  599; EM:  9; ED: 24; DayOffset: lotDecOne),
    (SY:  599; SM: 11; SD: 23; EY:  599; EM: 12; ED: 22; DayOffset: lotIncOne),
    (SY:  600; SM:  1; SD: 21; EY:  600; EM:  2; ED: 19; DayOffset: lotIncOne),
    (SY:  600; SM:  3; SD: 20; EY:  600; EM:  4; ED: 18; DayOffset: lotIncOne),
    (SY:  600; SM:  8; SD: 14; EY:  600; EM: 10; ED: 12; DayOffset: lotDecOne),
    (SY:  601; SM:  2; SD:  8; EY:  601; EM:  3; ED:  9; DayOffset: lotIncOne),
    (SY:  601; SM:  4; SD:  8; EY:  601; EM:  5; ED:  7; DayOffset: lotIncOne),
    (SY:  601; SM:  7; SD:  5; EY:  601; EM:  8; ED:  3; DayOffset: lotDecOne),
    (SY:  601; SM:  9; SD:  2; EY:  601; EM: 10; ED:  1; DayOffset: lotDecOne),
    (SY:  601; SM: 11; SD: 30; EY:  601; EM: 12; ED: 29; DayOffset: lotDecOne),
    (SY:  602; SM:  2; SD: 27; EY:  602; EM:  3; ED: 28; DayOffset: lotIncOne),
    (SY:  602; SM:  4; SD: 27; EY:  602; EM:  5; ED: 26; DayOffset: lotIncOne),
    (SY:  602; SM:  7; SD: 24; EY:  602; EM:  8; ED: 22; DayOffset: lotDecOne),
    (SY:  602; SM:  9; SD: 21; EY:  602; EM: 10; ED: 20; DayOffset: lotDecOne),
    (SY:  602; SM: 11; SD: 19; EY:  602; EM: 12; ED: 18; DayOffset: lotDecOne),
    (SY:  603; SM:  2; SD: 16; EY:  603; EM:  3; ED: 17; DayOffset: lotIncOne),
    (SY:  603; SM:  8; SD: 12; EY:  603; EM:  9; ED: 10; DayOffset: lotDecOne),
    (SY:  603; SM: 10; SD: 10; EY:  603; EM: 11; ED:  8; DayOffset: lotDecOne),
    (SY:  603; SM: 12; SD:  8; EY:  604; EM:  1; ED:  6; DayOffset: lotDecOne),
    (SY:  604; SM:  3; SD:  6; EY:  604; EM:  4; ED:  4; DayOffset: lotIncOne),
    (SY:  604; SM:  5; SD:  4; EY:  604; EM:  6; ED:  2; DayOffset: lotIncOne),
    (SY:  604; SM:  7; SD:  2; EY:  604; EM:  7; ED: 31; DayOffset: lotIncOne),
    (SY:  604; SM: 10; SD: 28; EY:  604; EM: 11; ED: 26; DayOffset: lotDecOne),
    (SY:  604; SM: 12; SD: 26; EY:  605; EM:  1; ED: 24; DayOffset: lotDecOne),
    (SY:  605; SM:  5; SD: 23; EY:  605; EM:  6; ED: 21; DayOffset: lotIncOne),
    (SY:  605; SM:  7; SD: 21; EY:  605; EM:  8; ED: 19; DayOffset: lotIncOne),
    (SY:  605; SM:  9; SD: 18; EY:  605; EM: 10; ED: 17; DayOffset: lotIncOne),
    (SY:  605; SM: 11; SD: 16; EY:  605; EM: 12; ED: 15; DayOffset: lotDecOne),
    (SY:  606; SM:  1; SD: 14; EY:  606; EM:  2; ED: 12; DayOffset: lotDecOne),
    (SY:  606; SM:  3; SD: 14; EY:  606; EM:  4; ED: 12; DayOffset: lotDecOne),
    (SY:  606; SM: 10; SD:  7; EY:  606; EM: 11; ED:  5; DayOffset: lotIncOne),
    (SY:  607; SM:  4; SD:  2; EY:  607; EM:  5; ED:  1; DayOffset: lotDecOne),
    (SY:  607; SM:  5; SD: 31; EY:  607; EM:  6; ED: 29; DayOffset: lotDecOne),
    (SY:  607; SM: 12; SD: 24; EY:  608; EM:  1; ED: 22; DayOffset: lotIncOne),
    (SY:  608; SM:  8; SD: 16; EY:  608; EM:  9; ED: 14; DayOffset: lotDecOne),
    (SY:  609; SM:  1; SD: 11; EY:  609; EM:  2; ED:  9; DayOffset: lotIncOne),
    (SY:  609; SM:  3; SD: 11; EY:  609; EM:  4; ED:  9; DayOffset: lotIncOne),
    (SY:  609; SM:  6; SD:  7; EY:  609; EM:  7; ED:  6; DayOffset: lotDecOne),
    (SY:  609; SM:  8; SD:  5; EY:  609; EM:  9; ED:  3; DayOffset: lotDecOne),
    (SY:  609; SM: 11; SD:  2; EY:  609; EM: 12; ED:  1; DayOffset: lotDecOne),
    (SY:  610; SM:  1; SD: 30; EY:  610; EM:  2; ED: 28; DayOffset: lotIncOne),
    (SY:  610; SM:  3; SD: 30; EY:  610; EM:  4; ED: 28; DayOffset: lotIncOne),
    (SY:  610; SM:  6; SD: 26; EY:  610; EM:  7; ED: 25; DayOffset: lotDecOne),
    (SY:  610; SM:  8; SD: 24; EY:  610; EM:  9; ED: 22; DayOffset: lotDecOne),
    (SY:  610; SM: 10; SD: 22; EY:  610; EM: 11; ED: 20; DayOffset: lotDecOne),
    (SY:  611; SM:  1; SD: 19; EY:  611; EM:  2; ED: 17; DayOffset: lotIncOne),
    (SY:  611; SM:  4; SD: 18; EY:  611; EM:  5; ED: 17; DayOffset: lotIncOne),
    (SY:  611; SM:  7; SD: 15; EY:  611; EM:  8; ED: 13; DayOffset: lotDecOne),
    (SY:  611; SM:  9; SD: 12; EY:  611; EM: 10; ED: 11; DayOffset: lotDecOne),
    (SY:  611; SM: 11; SD: 10; EY:  611; EM: 12; ED:  9; DayOffset: lotDecOne),
    (SY:  612; SM:  2; SD:  7; EY:  612; EM:  3; ED:  7; DayOffset: lotIncOne),
    (SY:  612; SM:  4; SD:  6; EY:  612; EM:  5; ED:  5; DayOffset: lotIncOne),
    (SY:  612; SM:  6; SD:  4; EY:  612; EM:  7; ED:  3; DayOffset: lotIncOne),
    (SY:  612; SM:  8; SD:  2; EY:  612; EM:  8; ED: 31; DayOffset: lotDecOne),
    (SY:  612; SM:  9; SD: 30; EY:  612; EM: 10; ED: 29; DayOffset: lotDecOne),
    (SY:  612; SM: 11; SD: 28; EY:  612; EM: 12; ED: 27; DayOffset: lotDecOne),
    (SY:  613; SM:  4; SD: 25; EY:  613; EM:  5; ED: 24; DayOffset: lotIncOne),
    (SY:  613; SM:  6; SD: 23; EY:  613; EM:  7; ED: 22; DayOffset: lotIncOne),
    (SY:  613; SM: 10; SD: 19; EY:  613; EM: 11; ED: 17; DayOffset: lotDecOne),
    (SY:  613; SM: 12; SD: 17; EY:  614; EM:  1; ED: 15; DayOffset: lotDecOne),
    (SY:  614; SM:  2; SD: 14; EY:  614; EM:  3; ED: 15; DayOffset: lotDecOne),
    (SY:  614; SM:  7; SD: 12; EY:  614; EM:  8; ED: 10; DayOffset: lotIncOne),
    (SY:  615; SM:  3; SD:  5; EY:  615; EM:  4; ED:  3; DayOffset: lotDecOne),
    (SY:  615; SM:  5; SD:  3; EY:  615; EM:  6; ED:  1; DayOffset: lotDecOne),
    (SY:  616; SM:  7; SD: 19; EY:  616; EM:  8; ED: 17; DayOffset: lotDecOne),
    (SY:  616; SM: 12; SD: 14; EY:  617; EM:  1; ED: 12; DayOffset: lotIncOne),
    (SY:  617; SM:  7; SD:  8; EY:  617; EM:  8; ED:  6; DayOffset: lotDecOne),
    (SY:  617; SM: 10; SD:  5; EY:  617; EM: 11; ED:  3; DayOffset: lotDecOne),
    (SY:  618; SM:  1; SD:  2; EY:  618; EM:  1; ED: 31; DayOffset: lotIncOne),
    (SY:  618; SM:  5; SD: 29; EY:  618; EM:  6; ED: 27; DayOffset: lotDecOne),
    (SY:  618; SM:  7; SD: 27; EY:  618; EM:  8; ED: 25; DayOffset: lotDecOne),
    (SY:  618; SM:  9; SD: 24; EY:  618; EM: 10; ED: 23; DayOffset: lotDecOne),
    (SY:  618; SM: 12; SD: 22; EY:  619; EM:  1; ED: 20; DayOffset: lotIncOne),
    (SY:  620; SM: 12; SD: 29; EY:  621; EM:  1; ED: 27; DayOffset: lotIncOne),
    (SY:  622; SM:  4; SD: 16; EY:  622; EM:  5; ED: 15; DayOffset: lotIncOne),
    (SY:  626; SM:  5; SD:  1; EY:  626; EM:  5; ED: 30; DayOffset: lotDecOne),
    (SY:  627; SM: 12; SD: 13; EY:  628; EM:  1; ED: 11; DayOffset: lotIncOne),
    (SY:  628; SM:  9; SD:  3; EY:  628; EM: 10; ED:  2; DayOffset: lotIncOne),
    (SY:  628; SM: 12; SD:  1; EY:  628; EM: 12; ED: 30; DayOffset: lotIncOne),
    (SY:  629; SM:  6; SD: 26; EY:  629; EM:  7; ED: 25; DayOffset: lotDecOne),
    (SY:  631; SM:  6; SD:  5; EY:  631; EM:  7; ED:  4; DayOffset: lotIncOne),
    (SY:  634; SM:  4; SD:  3; EY:  634; EM:  5; ED:  2; DayOffset: lotDecOne),
    (SY:  635; SM:  4; SD: 22; EY:  635; EM:  5; ED: 21; DayOffset: lotDecOne),
    (SY:  636; SM:  5; SD: 10; EY:  636; EM:  6; ED:  8; DayOffset: lotDecOne),
    (SY:  636; SM: 11; SD:  3; EY:  636; EM: 12; ED:  2; DayOffset: lotIncOne),
    (SY:  637; SM:  5; SD: 29; EY:  637; EM:  6; ED: 27; DayOffset: lotDecOne),
    (SY:  637; SM:  8; SD: 25; EY:  637; EM:  9; ED: 23; DayOffset: lotDecOne),
    (SY:  639; SM: 10; SD:  2; EY:  639; EM: 10; ED: 31; DayOffset: lotIncOne),
    (SY:  640; SM: 12; SD: 18; EY:  641; EM:  1; ED: 16; DayOffset: lotDecOne),
    (SY:  643; SM:  3; SD: 25; EY:  643; EM:  4; ED: 23; DayOffset: lotDecOne),
    (SY:  644; SM:  1; SD: 15; EY:  644; EM:  2; ED: 13; DayOffset: lotDecOne),
    (SY:  644; SM:  4; SD: 12; EY:  644; EM:  5; ED: 11; DayOffset: lotDecOne),
    (SY:  644; SM: 10; SD:  6; EY:  644; EM: 11; ED:  4; DayOffset: lotIncOne),
    (SY:  645; SM:  3; SD:  3; EY:  645; EM:  4; ED:  1; DayOffset: lotIncOne),
    (SY:  645; SM:  7; SD: 28; EY:  645; EM:  9; ED: 25; DayOffset: lotDecOne),
    (SY:  645; SM: 10; SD: 25; EY:  645; EM: 11; ED: 23; DayOffset: lotDecOne),
    (SY:  646; SM:  1; SD: 22; EY:  646; EM:  2; ED: 20; DayOffset: lotIncOne),
    (SY:  646; SM:  3; SD: 22; EY:  646; EM:  4; ED: 20; DayOffset: lotIncOne),
    (SY:  646; SM:  5; SD: 20; EY:  646; EM:  6; ED: 18; DayOffset: lotIncOne),
    (SY:  646; SM:  8; SD: 16; EY:  646; EM:  9; ED: 14; DayOffset: lotDecOne),
    (SY:  646; SM: 10; SD: 14; EY:  646; EM: 11; ED: 12; DayOffset: lotDecOne),
    (SY:  647; SM:  4; SD: 10; EY:  647; EM:  5; ED:  9; DayOffset: lotIncOne),
    (SY:  647; SM:  9; SD:  4; EY:  647; EM: 10; ED:  3; DayOffset: lotDecOne),
    (SY:  647; SM: 11; SD:  2; EY:  647; EM: 12; ED:  1; DayOffset: lotDecOne),
    (SY:  648; SM:  3; SD: 29; EY:  648; EM:  5; ED: 27; DayOffset: lotIncOne),
    (SY:  648; SM:  9; SD: 22; EY:  648; EM: 10; ED: 21; DayOffset: lotDecOne),
    (SY:  648; SM: 11; SD: 20; EY:  648; EM: 12; ED: 19; DayOffset: lotDecOne),
    (SY:  649; SM:  4; SD: 17; EY:  649; EM:  5; ED: 16; DayOffset: lotIncOne),
    (SY:  649; SM:  6; SD: 15; EY:  649; EM:  7; ED: 14; DayOffset: lotIncOne),
    (SY:  649; SM: 12; SD:  9; EY:  650; EM:  1; ED:  7; DayOffset: lotDecOne),
    (SY:  650; SM:  2; SD:  6; EY:  650; EM:  3; ED:  7; DayOffset: lotDecOne),
    (SY:  650; SM:  7; SD:  4; EY:  650; EM:  8; ED:  2; DayOffset: lotIncOne),
    (SY:  650; SM:  9; SD:  1; EY:  650; EM:  9; ED: 30; DayOffset: lotIncOne),
    (SY:  650; SM: 10; SD: 30; EY:  650; EM: 11; ED: 28; DayOffset: lotIncOne),
    (SY:  651; SM:  2; SD: 25; EY:  651; EM:  3; ED: 26; DayOffset: lotDecOne),
    (SY:  651; SM:  4; SD: 25; EY:  651; EM:  5; ED: 24; DayOffset: lotDecOne),
    (SY:  651; SM: 11; SD: 18; EY:  651; EM: 12; ED: 17; DayOffset: lotIncOne),
    (SY:  652; SM:  1; SD: 16; EY:  652; EM:  2; ED: 14; DayOffset: lotIncOne),
    (SY:  652; SM:  5; SD: 13; EY:  652; EM:  6; ED: 11; DayOffset: lotDecOne),
    (SY:  652; SM:  7; SD: 11; EY:  652; EM:  8; ED:  9; DayOffset: lotDecOne),
    (SY:  652; SM: 12; SD:  6; EY:  653; EM:  1; ED:  4; DayOffset: lotIncOne),
    (SY:  653; SM:  2; SD:  3; EY:  653; EM:  3; ED:  4; DayOffset: lotIncOne),
    (SY:  653; SM:  7; SD: 30; EY:  653; EM:  8; ED: 28; DayOffset: lotDecOne),
    (SY:  653; SM:  9; SD: 27; EY:  653; EM: 10; ED: 26; DayOffset: lotDecOne),
    (SY:  653; SM: 12; SD: 25; EY:  654; EM:  1; ED: 23; DayOffset: lotIncOne),
    (SY:  654; SM:  2; SD: 22; EY:  654; EM:  3; ED: 23; DayOffset: lotIncOne),
    (SY:  654; SM:  7; SD: 19; EY:  654; EM:  8; ED: 17; DayOffset: lotDecOne),
    (SY:  654; SM:  9; SD: 16; EY:  654; EM: 10; ED: 15; DayOffset: lotDecOne),
    (SY:  655; SM:  3; SD: 13; EY:  655; EM:  4; ED: 11; DayOffset: lotIncOne),
    (SY:  655; SM:  5; SD: 11; EY:  655; EM:  6; ED:  9; DayOffset: lotIncOne),
    (SY:  655; SM:  8; SD:  7; EY:  655; EM:  9; ED:  5; DayOffset: lotDecOne),
    (SY:  655; SM: 10; SD:  5; EY:  655; EM: 11; ED:  3; DayOffset: lotDecOne),
    (SY:  656; SM:  3; SD:  1; EY:  656; EM:  4; ED: 29; DayOffset: lotIncOne),
    (SY:  656; SM:  8; SD: 25; EY:  656; EM:  9; ED: 23; DayOffset: lotDecOne),
    (SY:  656; SM: 10; SD: 23; EY:  656; EM: 11; ED: 21; DayOffset: lotDecOne),
    (SY:  656; SM: 12; SD: 21; EY:  657; EM:  1; ED: 19; DayOffset: lotDecOne),
    (SY:  657; SM:  3; SD: 20; EY:  657; EM:  4; ED: 18; DayOffset: lotIncOne),
    (SY:  657; SM:  5; SD: 18; EY:  657; EM:  6; ED: 16; DayOffset: lotIncOne),
    (SY:  657; SM:  9; SD: 13; EY:  657; EM: 10; ED: 12; DayOffset: lotDecOne),
    (SY:  657; SM: 11; SD: 11; EY:  657; EM: 12; ED: 10; DayOffset: lotDecOne),
    (SY:  658; SM:  1; SD:  9; EY:  658; EM:  2; ED:  7; DayOffset: lotDecOne),
    (SY:  658; SM:  6; SD:  6; EY:  658; EM:  7; ED:  5; DayOffset: lotIncOne),
    (SY:  658; SM:  8; SD:  4; EY:  658; EM:  9; ED:  2; DayOffset: lotIncOne),
    (SY:  658; SM: 10; SD:  2; EY:  658; EM: 10; ED: 31; DayOffset: lotIncOne),
    (SY:  658; SM: 11; SD: 30; EY:  658; EM: 12; ED: 29; DayOffset: lotDecOne),
    (SY:  659; SM:  1; SD: 28; EY:  659; EM:  2; ED: 26; DayOffset: lotDecOne),
    (SY:  659; SM:  3; SD: 28; EY:  659; EM:  4; ED: 26; DayOffset: lotDecOne),
    (SY:  659; SM:  8; SD: 23; EY:  659; EM:  9; ED: 21; DayOffset: lotIncOne),
    (SY:  659; SM: 10; SD: 21; EY:  659; EM: 11; ED: 19; DayOffset: lotIncOne),
    (SY:  660; SM:  4; SD: 15; EY:  660; EM:  5; ED: 14; DayOffset: lotDecOne),
    (SY:  660; SM:  6; SD: 13; EY:  660; EM:  7; ED: 12; DayOffset: lotDecOne),
    (SY:  660; SM: 11; SD:  8; EY:  660; EM: 12; ED:  7; DayOffset: lotIncOne),
    (SY:  661; SM:  1; SD:  6; EY:  661; EM:  2; ED:  4; DayOffset: lotIncOne),
    (SY:  661; SM:  7; SD:  2; EY:  661; EM:  7; ED: 31; DayOffset: lotDecOne),
    (SY:  661; SM:  8; SD: 30; EY:  661; EM:  9; ED: 28; DayOffset: lotDecOne),
    (SY:  661; SM: 11; SD: 27; EY:  661; EM: 12; ED: 26; DayOffset: lotIncOne),
    (SY:  662; SM:  1; SD: 25; EY:  662; EM:  2; ED: 23; DayOffset: lotIncOne),
    (SY:  662; SM:  6; SD: 21; EY:  662; EM:  7; ED: 20; DayOffset: lotDecOne),
    (SY:  662; SM:  8; SD: 19; EY:  662; EM:  9; ED: 17; DayOffset: lotDecOne),
    (SY:  663; SM:  2; SD: 13; EY:  663; EM:  3; ED: 14; DayOffset: lotIncOne),
    (SY:  663; SM:  7; SD: 10; EY:  663; EM:  8; ED:  8; DayOffset: lotDecOne),
    (SY:  663; SM:  9; SD:  7; EY:  663; EM: 10; ED:  6; DayOffset: lotDecOne),
    (SY:  664; SM:  2; SD:  2; EY:  664; EM:  4; ED:  1; DayOffset: lotIncOne),
    (SY:  664; SM:  7; SD: 28; EY:  664; EM:  8; ED: 26; DayOffset: lotDecOne),
    (SY:  664; SM:  9; SD: 25; EY:  664; EM: 10; ED: 24; DayOffset: lotDecOne),
    (SY:  664; SM: 11; SD: 23; EY:  664; EM: 12; ED: 22; DayOffset: lotDecOne),
    (SY:  665; SM:  2; SD: 20; EY:  665; EM:  3; ED: 21; DayOffset: lotIncOne),
    (SY:  665; SM:  4; SD: 20; EY:  665; EM:  5; ED: 19; DayOffset: lotIncOne),
    (SY:  665; SM:  8; SD: 16; EY:  665; EM:  9; ED: 14; DayOffset: lotDecOne),
    (SY:  665; SM: 10; SD: 14; EY:  665; EM: 11; ED: 12; DayOffset: lotDecOne),
    (SY:  665; SM: 12; SD: 12; EY:  666; EM:  1; ED: 10; DayOffset: lotDecOne),
    (SY:  666; SM:  3; SD: 11; EY:  666; EM:  4; ED:  9; DayOffset: lotDecOne),
    (SY:  667; SM:  5; SD: 28; EY:  667; EM:  6; ED: 26; DayOffset: lotDecOne),
    (SY:  668; SM: 10; SD: 11; EY:  668; EM: 11; ED:  9; DayOffset: lotIncOne),
    (SY:  672; SM:  4; SD:  3; EY:  672; EM:  5; ED:  2; DayOffset: lotDecOne),
    (SY:  680; SM:  1; SD:  7; EY:  680; EM:  2; ED:  5; DayOffset: lotIncOne),
    (SY:  682; SM: 10; SD:  6; EY:  682; EM: 11; ED:  4; DayOffset: lotIncOne),
    (SY:  683; SM:  1; SD:  3; EY:  683; EM:  2; ED:  1; DayOffset: lotIncOne),
    (SY:  683; SM: 10; SD: 25; EY:  683; EM: 11; ED: 23; DayOffset: lotIncOne),
    (SY:  684; SM:  1; SD: 22; EY:  684; EM:  2; ED: 20; DayOffset: lotDecOne),
    (SY:  687; SM:  6; SD: 15; EY:  687; EM:  7; ED: 14; DayOffset: lotDecOne),
    (SY:  690; SM: 12; SD:  6; EY:  691; EM:  1; ED:  4; DayOffset: lotIncOne),
    (SY:  697; SM: 11; SD: 19; EY:  698; EM:  1; ED: 17; DayOffset: lotDecOne),
    (SY:  698; SM:  8; SD: 11; EY:  698; EM:  9; ED:  9; DayOffset: lotDecOne),
    (SY:  704; SM:  5; SD:  8; EY:  704; EM:  6; ED:  6; DayOffset: lotDecOne),
    (SY:  706; SM: 10; SD: 11; EY:  706; EM: 11; ED:  9; DayOffset: lotDecOne),
    (SY:  708; SM: 10; SD: 18; EY:  708; EM: 11; ED: 16; DayOffset: lotDecOne),
    (SY:  709; SM:  2; SD: 14; EY:  709; EM:  3; ED: 15; DayOffset: lotDecOne),
    (SY:  709; SM:  9; SD:  8; EY:  709; EM: 10; ED:  7; DayOffset: lotDecOne),
    (SY:  710; SM:  2; SD:  3; EY:  710; EM:  3; ED:  4; DayOffset: lotDecOne),
    (SY:  710; SM:  5; SD:  3; EY:  710; EM:  6; ED:  1; DayOffset: lotDecOne),
    (SY:  710; SM:  7; SD:  1; EY:  710; EM:  7; ED: 30; DayOffset: lotDecOne),
    (SY:  710; SM:  9; SD: 27; EY:  710; EM: 10; ED: 26; DayOffset: lotDecOne),
    (SY:  710; SM: 11; SD: 25; EY:  710; EM: 12; ED: 24; DayOffset: lotDecOne),
    (SY:  712; SM:  2; SD: 11; EY:  712; EM:  3; ED: 11; DayOffset: lotDecOne),
    (SY:  712; SM:  7; SD:  8; EY:  712; EM:  8; ED:  6; DayOffset: lotDecOne),
    (SY:  713; SM:  4; SD: 29; EY:  713; EM:  5; ED: 28; DayOffset: lotDecOne),
    (SY:  713; SM:  6; SD: 27; EY:  713; EM:  7; ED: 26; DayOffset: lotDecOne),
    (SY:  714; SM:  3; SD: 20; EY:  714; EM:  4; ED: 18; DayOffset: lotDecOne),
    (SY:  715; SM:  4; SD:  8; EY:  715; EM:  5; ED:  7; DayOffset: lotDecOne),
    (SY:  715; SM:  7; SD:  5; EY:  715; EM:  8; ED:  3; DayOffset: lotDecOne),
    (SY:  716; SM:  4; SD: 26; EY:  716; EM:  5; ED: 25; DayOffset: lotDecOne),
    (SY:  716; SM:  7; SD: 23; EY:  716; EM:  8; ED: 21; DayOffset: lotDecOne),
    (SY:  716; SM:  9; SD: 20; EY:  716; EM: 10; ED: 19; DayOffset: lotDecOne),
    (SY:  717; SM:  1; SD: 17; EY:  717; EM:  2; ED: 15; DayOffset: lotDecOne),
    (SY:  718; SM:  8; SD: 30; EY:  718; EM:  9; ED: 28; DayOffset: lotDecOne),
    (SY:  718; SM: 12; SD: 26; EY:  719; EM:  2; ED: 23; DayOffset: lotDecOne),
    (SY:  721; SM:  6; SD: 29; EY:  721; EM:  7; ED: 28; DayOffset: lotDecOne),
    (SY:  722; SM:  6; SD: 18; EY:  722; EM:  7; ED: 17; DayOffset: lotDecOne),
    (SY:  722; SM:  9; SD: 15; EY:  722; EM: 10; ED: 14; DayOffset: lotDecOne),
    (SY:  723; SM:  3; SD: 11; EY:  723; EM:  4; ED:  9; DayOffset: lotDecOne),
    (SY:  723; SM:  9; SD:  4; EY:  723; EM: 10; ED:  3; DayOffset: lotDecOne),
    (SY:  723; SM: 12; SD:  2; EY:  723; EM: 12; ED: 31; DayOffset: lotDecOne),
    (SY:  724; SM:  8; SD: 23; EY:  724; EM:  9; ED: 21; DayOffset: lotDecOne),
    (SY:  724; SM: 12; SD: 20; EY:  725; EM:  1; ED: 18; DayOffset: lotDecOne),
    (SY:  725; SM:  4; SD: 17; EY:  725; EM:  5; ED: 16; DayOffset: lotDecOne),
    (SY:  725; SM:  7; SD: 14; EY:  725; EM:  8; ED: 12; DayOffset: lotDecOne),
    (SY:  725; SM: 12; SD:  9; EY:  726; EM:  1; ED:  7; DayOffset: lotDecOne),
    (SY:  726; SM:  5; SD:  6; EY:  726; EM:  6; ED:  4; DayOffset: lotDecOne),
    (SY:  726; SM: 11; SD: 28; EY:  726; EM: 12; ED: 27; DayOffset: lotDecOne),
    (SY:  727; SM:  5; SD: 25; EY:  727; EM:  6; ED: 23; DayOffset: lotDecOne),
    (SY:  727; SM:  8; SD: 21; EY:  727; EM:  9; ED: 19; DayOffset: lotDecOne),
    (SY:  727; SM: 12; SD: 17; EY:  728; EM:  1; ED: 15; DayOffset: lotDecOne),
    (SY:  729; SM:  3; SD:  4; EY:  729; EM:  4; ED:  2; DayOffset: lotDecOne),
    (SY:  729; SM:  7; SD: 30; EY:  729; EM:  8; ED: 28; DayOffset: lotIncOne),
    (SY:  732; SM:  5; SD: 28; EY:  732; EM:  6; ED: 26; DayOffset: lotDecOne),
    (SY:  732; SM:  7; SD: 26; EY:  732; EM:  8; ED: 24; DayOffset: lotDecOne),
    (SY:  733; SM:  3; SD: 20; EY:  733; EM:  4; ED: 18; DayOffset: lotDecOne),
    (SY:  733; SM:  6; SD: 16; EY:  733; EM:  7; ED: 15; DayOffset: lotDecOne),
    (SY:  736; SM:  2; SD: 16; EY:  736; EM:  3; ED: 16; DayOffset: lotIncOne),
    (SY:  737; SM:  2; SD:  4; EY:  737; EM:  3; ED:  5; DayOffset: lotIncOne),
    (SY:  737; SM:  8; SD: 30; EY:  737; EM:  9; ED: 28; DayOffset: lotDecOne),
    (SY:  738; SM:  7; SD: 21; EY:  738; EM:  8; ED: 19; DayOffset: lotDecOne),
    (SY:  738; SM:  9; SD: 18; EY:  738; EM: 10; ED: 17; DayOffset: lotDecOne),
    (SY:  741; SM:  2; SD: 20; EY:  741; EM:  3; ED: 21; DayOffset: lotDecOne),
    (SY:  741; SM:  5; SD: 19; EY:  741; EM:  6; ED: 17; DayOffset: lotDecOne),
    (SY:  741; SM:  7; SD: 17; EY:  741; EM:  8; ED: 15; DayOffset: lotDecOne),
    (SY:  741; SM: 10; SD: 14; EY:  741; EM: 11; ED: 12; DayOffset: lotDecOne),
    (SY:  742; SM:  1; SD: 11; EY:  742; EM:  2; ED:  9; DayOffset: lotDecOne),
    (SY:  742; SM:  3; SD: 11; EY:  742; EM:  4; ED:  9; DayOffset: lotDecOne),
    (SY:  742; SM:  6; SD:  7; EY:  742; EM:  7; ED:  6; DayOffset: lotDecOne),
    (SY:  742; SM: 10; SD:  3; EY:  742; EM: 11; ED:  1; DayOffset: lotDecOne),
    (SY:  743; SM:  3; SD: 30; EY:  743; EM:  4; ED: 28; DayOffset: lotDecOne),
    (SY:  743; SM:  6; SD: 26; EY:  743; EM:  7; ED: 25; DayOffset: lotDecOne),
    (SY:  744; SM:  4; SD: 17; EY:  744; EM:  5; ED: 16; DayOffset: lotDecOne),
    (SY:  744; SM:  7; SD: 14; EY:  744; EM:  8; ED: 12; DayOffset: lotDecOne),
    (SY:  744; SM: 10; SD: 10; EY:  744; EM: 11; ED:  8; DayOffset: lotDecOne),
    (SY:  745; SM:  8; SD:  2; EY:  745; EM:  8; ED: 31; DayOffset: lotDecOne),
    (SY:  746; SM:  4; SD: 25; EY:  746; EM:  5; ED: 24; DayOffset: lotDecOne),
    (SY:  746; SM:  8; SD: 21; EY:  746; EM:  9; ED: 19; DayOffset: lotDecOne),
    (SY:  747; SM:  4; SD: 14; EY:  747; EM:  5; ED: 13; DayOffset: lotDecOne),
    (SY:  748; SM:  6; SD: 30; EY:  748; EM:  7; ED: 29; DayOffset: lotDecOne),
    (SY:  749; SM:  4; SD: 21; EY:  749; EM:  5; ED: 20; DayOffset: lotDecOne),
    (SY:  749; SM:  6; SD: 19; EY:  749; EM:  7; ED: 18; DayOffset: lotDecOne),
    (SY:  750; SM:  5; SD: 10; EY:  750; EM:  6; ED:  8; DayOffset: lotDecOne),
    (SY:  750; SM:  9; SD:  5; EY:  750; EM: 10; ED:  4; DayOffset: lotDecOne),
    (SY:  751; SM:  1; SD:  2; EY:  751; EM:  1; ED: 31; DayOffset: lotDecOne),
    (SY:  751; SM:  3; SD:  2; EY:  751; EM:  3; ED: 31; DayOffset: lotDecOne),
    (SY:  751; SM:  5; SD: 29; EY:  751; EM:  6; ED: 27; DayOffset: lotDecOne),
    (SY:  752; SM:  9; SD: 12; EY:  752; EM: 10; ED: 11; DayOffset: lotDecOne),
    (SY:  752; SM: 12; SD: 10; EY:  753; EM:  1; ED:  8; DayOffset: lotDecOne),
    (SY:  753; SM: 10; SD:  1; EY:  753; EM: 10; ED: 30; DayOffset: lotDecOne),
    (SY:  754; SM: 10; SD: 20; EY:  754; EM: 11; ED: 18; DayOffset: lotDecOne),
    (SY:  755; SM:  3; SD: 17; EY:  755; EM:  4; ED: 15; DayOffset: lotIncOne),
    (SY:  757; SM:  3; SD: 24; EY:  757; EM:  4; ED: 22; DayOffset: lotDecOne),
    (SY:  757; SM:  8; SD: 19; EY:  757; EM:  9; ED: 17; DayOffset: lotDecOne),
    (SY:  758; SM:  4; SD: 12; EY:  758; EM:  5; ED: 11; DayOffset: lotDecOne),
    (SY:  758; SM:  6; SD: 10; EY:  758; EM:  7; ED:  9; DayOffset: lotDecOne),
    (SY:  758; SM: 12; SD:  5; EY:  759; EM:  1; ED:  3; DayOffset: lotDecOne),
    (SY:  759; SM:  2; SD:  2; EY:  759; EM:  3; ED:  3; DayOffset: lotDecOne),
    (SY:  759; SM:  5; SD:  1; EY:  759; EM:  5; ED: 30; DayOffset: lotDecOne),
    (SY:  759; SM: 11; SD: 24; EY:  759; EM: 12; ED: 23; DayOffset: lotDecOne),
    (SY:  760; SM:  2; SD: 21; EY:  760; EM:  3; ED: 21; DayOffset: lotDecOne),
    (SY:  760; SM:  5; SD: 19; EY:  760; EM:  6; ED: 17; DayOffset: lotDecOne),
    (SY:  760; SM:  8; SD: 15; EY:  760; EM:  9; ED: 13; DayOffset: lotDecOne),
    (SY:  760; SM: 11; SD: 12; EY:  760; EM: 12; ED: 11; DayOffset: lotDecOne),
    (SY:  761; SM:  6; SD:  7; EY:  761; EM:  7; ED:  6; DayOffset: lotDecOne),
    (SY:  761; SM:  9; SD:  3; EY:  761; EM: 10; ED:  2; DayOffset: lotDecOne),
    (SY:  761; SM: 12; SD:  1; EY:  761; EM: 12; ED: 30; DayOffset: lotDecOne),
    (SY:  762; SM:  6; SD: 26; EY:  762; EM:  7; ED: 25; DayOffset: lotDecOne),
    (SY:  762; SM:  9; SD: 22; EY:  762; EM: 10; ED: 21; DayOffset: lotDecOne),
    (SY:  762; SM: 11; SD: 20; EY:  762; EM: 12; ED: 19; DayOffset: lotDecOne),
    (SY:  763; SM:  7; SD: 15; EY:  763; EM:  8; ED: 13; DayOffset: lotDecOne),
    (SY:  763; SM: 10; SD: 11; EY:  763; EM: 11; ED:  9; DayOffset: lotDecOne),
    (SY:  763; SM: 12; SD:  9; EY:  764; EM:  1; ED:  7; DayOffset: lotDecOne),
    (SY:  764; SM:  3; SD:  7; EY:  764; EM:  4; ED:  5; DayOffset: lotDecOne),
    (SY:  764; SM: 12; SD: 27; EY:  765; EM:  1; ED: 25; DayOffset: lotDecOne),
    (SY:  765; SM:  5; SD: 24; EY:  765; EM:  6; ED: 22; DayOffset: lotDecOne),
    (SY:  766; SM:  3; SD: 15; EY:  766; EM:  4; ED: 13; DayOffset: lotDecOne),
    (SY:  766; SM:  5; SD: 13; EY:  766; EM:  6; ED: 11; DayOffset: lotDecOne),
    (SY:  767; SM:  4; SD:  3; EY:  767; EM:  5; ED:  2; DayOffset: lotDecOne),
    (SY:  767; SM: 10; SD: 27; EY:  767; EM: 11; ED: 25; DayOffset: lotDecOne),
    (SY:  768; SM:  4; SD: 21; EY:  768; EM:  5; ED: 20; DayOffset: lotDecOne),
    (SY:  768; SM:  7; SD: 18; EY:  768; EM:  8; ED: 16; DayOffset: lotDecOne),
    (SY:  768; SM: 10; SD: 15; EY:  768; EM: 11; ED: 13; DayOffset: lotDecOne),
    (SY:  769; SM:  5; SD: 10; EY:  769; EM:  6; ED:  8; DayOffset: lotDecOne),
    (SY:  770; SM:  5; SD: 29; EY:  770; EM:  6; ED: 27; DayOffset: lotDecOne),
    (SY:  770; SM:  8; SD: 25; EY:  770; EM:  9; ED: 23; DayOffset: lotDecOne),
    (SY:  772; SM:  2; SD:  8; EY:  772; EM:  3; ED:  8; DayOffset: lotDecOne),
    (SY:  772; SM: 10; SD:  1; EY:  772; EM: 10; ED: 30; DayOffset: lotDecOne),
    (SY:  773; SM:  1; SD: 27; EY:  773; EM:  2; ED: 25; DayOffset: lotDecOne),
    (SY:  773; SM: 10; SD: 20; EY:  773; EM: 11; ED: 18; DayOffset: lotDecOne),
    (SY:  773; SM: 12; SD: 18; EY:  774; EM:  1; ED: 16; DayOffset: lotDecOne),
    (SY:  774; SM:  7; SD: 13; EY:  774; EM:  8; ED: 11; DayOffset: lotDecOne),
    (SY:  775; SM:  1; SD:  6; EY:  775; EM:  2; ED:  4; DayOffset: lotDecOne),
    (SY:  775; SM:  3; SD:  6; EY:  775; EM:  4; ED:  4; DayOffset: lotDecOne),
    (SY:  775; SM:  7; SD:  2; EY:  775; EM:  7; ED: 31; DayOffset: lotDecOne),
    (SY:  775; SM:  9; SD: 29; EY:  775; EM: 10; ED: 28; DayOffset: lotDecOne),
    (SY:  777; SM:  4; SD: 12; EY:  777; EM:  5; ED: 11; DayOffset: lotDecOne),
    (SY:  777; SM:  7; SD:  9; EY:  777; EM:  8; ED:  7; DayOffset: lotDecOne),
    (SY:  778; SM:  5; SD:  1; EY:  778; EM:  5; ED: 30; DayOffset: lotDecOne),
    (SY:  778; SM:  7; SD: 28; EY:  778; EM:  8; ED: 26; DayOffset: lotDecOne),
    (SY:  778; SM:  9; SD: 25; EY:  778; EM: 10; ED: 24; DayOffset: lotDecOne),
    (SY:  779; SM:  5; SD: 20; EY:  779; EM:  6; ED: 18; DayOffset: lotDecOne),
    (SY:  779; SM: 10; SD: 14; EY:  779; EM: 11; ED: 12; DayOffset: lotDecOne),
    (SY:  780; SM:  1; SD: 11; EY:  780; EM:  3; ED: 10; DayOffset: lotDecOne),
    (SY:  780; SM: 12; SD: 30; EY:  781; EM:  1; ED: 28; DayOffset: lotDecOne),
    (SY:  782; SM:  1; SD: 18; EY:  782; EM:  2; ED: 16; DayOffset: lotDecOne),
    (SY:  782; SM: 10; SD: 11; EY:  782; EM: 11; ED:  9; DayOffset: lotDecOne),
    (SY:  783; SM:  4; SD:  6; EY:  783; EM:  5; ED:  5; DayOffset: lotDecOne),
    (SY:  783; SM:  9; SD:  1; EY:  783; EM:  9; ED: 30; DayOffset: lotDecOne),
    (SY:  783; SM: 10; SD: 30; EY:  783; EM: 11; ED: 28; DayOffset: lotDecOne),
    (SY:  783; SM: 12; SD: 28; EY:  784; EM:  1; ED: 26; DayOffset: lotDecOne),
    (SY:  785; SM: 12; SD:  6; EY:  786; EM:  1; ED:  4; DayOffset: lotDecOne),
    (SY:  787; SM:  4; SD: 22; EY:  787; EM:  5; ED: 21; DayOffset: lotDecOne),
    (SY:  787; SM: 12; SD: 14; EY:  788; EM:  1; ED: 12; DayOffset: lotDecOne),
    (SY:  788; SM: 12; SD:  2; EY:  788; EM: 12; ED: 31; DayOffset: lotDecOne),
    (SY:  790; SM:  3; SD: 20; EY:  790; EM:  4; ED: 18; DayOffset: lotDecOne),
    (SY:  791; SM:  3; SD:  9; EY:  791; EM:  4; ED:  7; DayOffset: lotDecOne),
    (SY:  791; SM: 10; SD:  2; EY:  791; EM: 10; ED: 31; DayOffset: lotDecOne),
    (SY:  792; SM:  5; SD: 25; EY:  792; EM:  6; ED: 23; DayOffset: lotDecOne),
    (SY:  792; SM: 10; SD: 20; EY:  792; EM: 11; ED: 18; DayOffset: lotDecOne),
    (SY:  792; SM: 12; SD: 18; EY:  793; EM:  1; ED: 16; DayOffset: lotDecOne),
    (SY:  793; SM: 11; SD:  8; EY:  793; EM: 12; ED:  7; DayOffset: lotDecOne),
    (SY:  794; SM:  1; SD:  6; EY:  794; EM:  2; ED:  4; DayOffset: lotDecOne),
    (SY:  794; SM:  6; SD:  2; EY:  794; EM:  7; ED:  1; DayOffset: lotDecOne),
    (SY:  794; SM:  7; SD: 31; EY:  794; EM:  8; ED: 29; DayOffset: lotDecOne),
    (SY:  795; SM:  3; SD: 25; EY:  795; EM:  4; ED: 23; DayOffset: lotDecOne),
    (SY:  795; SM:  6; SD: 21; EY:  795; EM:  7; ED: 20; DayOffset: lotDecOne),
    (SY:  796; SM:  4; SD: 12; EY:  796; EM:  5; ED: 11; DayOffset: lotDecOne),
    (SY:  799; SM:  2; SD:  9; EY:  799; EM:  3; ED: 10; DayOffset: lotDecOne),
    (SY:  802; SM:  5; SD:  5; EY:  802; EM:  6; ED:  3; DayOffset: lotDecOne),
    (SY:  802; SM: 12; SD: 28; EY:  803; EM:  1; ED: 26; DayOffset: lotDecOne),
    (SY:  804; SM:  1; SD: 16; EY:  804; EM:  2; ED: 14; DayOffset: lotDecOne),
    (SY:  806; SM: 10; SD: 15; EY:  806; EM: 11; ED: 13; DayOffset: lotDecOne),
    (SY:  807; SM:  1; SD: 12; EY:  807; EM:  2; ED: 10; DayOffset: lotDecOne),
    (SY:  807; SM: 11; SD:  3; EY:  807; EM: 12; ED:  2; DayOffset: lotDecOne),
    (SY:  808; SM:  4; SD: 29; EY:  808; EM:  5; ED: 28; DayOffset: lotDecOne),
    (SY:  808; SM:  6; SD: 27; EY:  808; EM:  7; ED: 26; DayOffset: lotDecOne),
    (SY:  808; SM:  8; SD: 25; EY:  808; EM:  9; ED: 23; DayOffset: lotDecOne),
    (SY:  809; SM:  4; SD: 18; EY:  809; EM:  5; ED: 17; DayOffset: lotDecOne),
    (SY:  810; SM:  7; SD:  5; EY:  810; EM:  8; ED:  3; DayOffset: lotDecOne),
    (SY:  810; SM: 11; SD: 30; EY:  810; EM: 12; ED: 29; DayOffset: lotDecOne),
    (SY:  811; SM:  4; SD: 26; EY:  811; EM:  5; ED: 25; DayOffset: lotDecOne),
    (SY:  811; SM:  6; SD: 24; EY:  811; EM:  7; ED: 23; DayOffset: lotDecOne),
    (SY:  811; SM:  9; SD: 21; EY:  811; EM: 10; ED: 20; DayOffset: lotDecOne),
    (SY:  811; SM: 12; SD: 19; EY:  812; EM:  1; ED: 17; DayOffset: lotDecOne),
    (SY:  812; SM:  2; SD: 16; EY:  812; EM:  3; ED: 16; DayOffset: lotDecOne),
    (SY:  812; SM:  9; SD:  9; EY:  812; EM: 10; ED:  8; DayOffset: lotDecOne),
    (SY:  813; SM:  1; SD:  6; EY:  813; EM:  2; ED:  4; DayOffset: lotDecOne),
    (SY:  813; SM:  3; SD:  6; EY:  813; EM:  4; ED:  4; DayOffset: lotDecOne),
    (SY:  814; SM:  3; SD: 25; EY:  814; EM:  4; ED: 23; DayOffset: lotDecOne),
    (SY:  814; SM:  9; SD: 17; EY:  814; EM: 10; ED: 16; DayOffset: lotDecOne),
    (SY:  814; SM: 12; SD: 15; EY:  815; EM:  2; ED: 12; DayOffset: lotDecOne),
    (SY:  815; SM:  7; SD: 10; EY:  815; EM:  8; ED:  8; DayOffset: lotDecOne),
    (SY:  815; SM: 10; SD:  6; EY:  815; EM: 11; ED:  4; DayOffset: lotDecOne),
    (SY:  816; SM:  1; SD:  3; EY:  816; EM:  2; ED:  1; DayOffset: lotDecOne),
    (SY:  816; SM:  7; SD: 28; EY:  816; EM:  8; ED: 26; DayOffset: lotDecOne),
    (SY:  816; SM: 10; SD: 24; EY:  816; EM: 11; ED: 22; DayOffset: lotDecOne),
    (SY:  816; SM: 12; SD: 22; EY:  817; EM:  1; ED: 20; DayOffset: lotDecOne),
    (SY:  817; SM:  6; SD: 18; EY:  817; EM:  7; ED: 17; DayOffset: lotDecOne),
    (SY:  818; SM:  1; SD: 10; EY:  818; EM:  2; ED:  8; DayOffset: lotDecOne),
    (SY:  819; SM:  3; SD: 29; EY:  819; EM:  4; ED: 27; DayOffset: lotDecOne),
    (SY:  819; SM:  8; SD: 24; EY:  819; EM:  9; ED: 22; DayOffset: lotDecOne),
    (SY:  820; SM:  4; SD: 16; EY:  820; EM:  5; ED: 15; DayOffset: lotDecOne),
    (SY:  820; SM:  6; SD: 14; EY:  820; EM:  7; ED: 13; DayOffset: lotDecOne),
    (SY:  820; SM:  8; SD: 12; EY:  820; EM:  9; ED: 10; DayOffset: lotDecOne),
    (SY:  821; SM:  5; SD:  5; EY:  821; EM:  6; ED:  3; DayOffset: lotDecOne),
    (SY:  821; SM: 11; SD: 28; EY:  821; EM: 12; ED: 27; DayOffset: lotDecOne),
    (SY:  822; SM:  2; SD: 25; EY:  822; EM:  3; ED: 26; DayOffset: lotDecOne),
    (SY:  822; SM:  5; SD: 24; EY:  822; EM:  6; ED: 22; DayOffset: lotDecOne),
    (SY:  822; SM:  8; SD: 20; EY:  822; EM:  9; ED: 18; DayOffset: lotDecOne),
    (SY:  822; SM: 11; SD: 17; EY:  822; EM: 12; ED: 16; DayOffset: lotDecOne),
    (SY:  823; SM:  6; SD: 12; EY:  823; EM:  7; ED: 11; DayOffset: lotDecOne),
    (SY:  823; SM:  9; SD:  8; EY:  823; EM: 10; ED:  7; DayOffset: lotDecOne),
    (SY:  823; SM: 11; SD:  6; EY:  823; EM: 12; ED:  5; DayOffset: lotDecOne),
    (SY:  824; SM:  6; SD: 30; EY:  824; EM:  7; ED: 29; DayOffset: lotDecOne),
    (SY:  824; SM: 11; SD: 24; EY:  824; EM: 12; ED: 23; DayOffset: lotDecOne),
    (SY:  825; SM:  3; SD: 23; EY:  825; EM:  4; ED: 21; DayOffset: lotDecOne),
    (SY:  825; SM:  5; SD: 21; EY:  825; EM:  6; ED: 19; DayOffset: lotDecOne),
    (SY:  825; SM:  7; SD: 19; EY:  825; EM:  8; ED: 17; DayOffset: lotDecOne),
    (SY:  825; SM: 10; SD: 15; EY:  825; EM: 11; ED: 13; DayOffset: lotDecOne),
    (SY:  825; SM: 12; SD: 13; EY:  826; EM:  1; ED: 11; DayOffset: lotDecOne),
    (SY:  826; SM:  3; SD: 12; EY:  826; EM:  4; ED: 10; DayOffset: lotDecOne),
    (SY:  826; SM:  6; SD:  9; EY:  826; EM:  7; ED:  8; DayOffset: lotDecOne),
    (SY:  826; SM: 11; SD:  3; EY:  826; EM: 12; ED:  2; DayOffset: lotDecOne),
    (SY:  827; SM:  1; SD:  1; EY:  827; EM:  1; ED: 30; DayOffset: lotDecOne),
    (SY:  827; SM:  3; SD:  1; EY:  827; EM:  3; ED: 30; DayOffset: lotDecOne),
    (SY:  827; SM:  5; SD: 29; EY:  827; EM:  6; ED: 27; DayOffset: lotDecOne),
    (SY:  828; SM:  1; SD: 20; EY:  828; EM:  2; ED: 18; DayOffset: lotDecOne),
    (SY:  828; SM:  3; SD: 19; EY:  828; EM:  4; ED: 17; DayOffset: lotDecOne),
    (SY:  828; SM:  5; SD: 17; EY:  828; EM:  6; ED: 15; DayOffset: lotDecOne),
    (SY:  828; SM:  8; SD: 14; EY:  828; EM:  9; ED: 12; DayOffset: lotDecOne),
    (SY:  829; SM:  4; SD:  7; EY:  829; EM:  5; ED:  6; DayOffset: lotDecOne),
    (SY:  829; SM:  8; SD:  3; EY:  829; EM:  9; ED:  1; DayOffset: lotDecOne),
    (SY:  829; SM: 10; SD: 31; EY:  829; EM: 11; ED: 29; DayOffset: lotDecOne),
    (SY:  830; SM:  4; SD: 26; EY:  830; EM:  5; ED: 25; DayOffset: lotDecOne),
    (SY:  830; SM:  7; SD: 23; EY:  830; EM:  8; ED: 21; DayOffset: lotDecOne),
    (SY:  830; SM: 10; SD: 20; EY:  830; EM: 11; ED: 18; DayOffset: lotDecOne),
    (SY:  831; SM:  5; SD: 15; EY:  831; EM:  6; ED: 13; DayOffset: lotDecOne),
    (SY:  831; SM:  8; SD: 11; EY:  831; EM:  9; ED:  9; DayOffset: lotDecOne),
    (SY:  831; SM: 10; SD:  9; EY:  831; EM: 11; ED:  7; DayOffset: lotDecOne),
    (SY:  832; SM:  6; SD:  2; EY:  832; EM:  7; ED:  1; DayOffset: lotDecOne),
    (SY:  832; SM:  8; SD: 29; EY:  832; EM:  9; ED: 27; DayOffset: lotDecOne),
    (SY:  832; SM: 10; SD: 27; EY:  832; EM: 11; ED: 25; DayOffset: lotDecOne),
    (SY:  833; SM:  2; SD: 23; EY:  833; EM:  3; ED: 24; DayOffset: lotDecOne),
    (SY:  833; SM:  6; SD: 21; EY:  833; EM:  7; ED: 20; DayOffset: lotDecOne),
    (SY:  834; SM:  2; SD: 12; EY:  834; EM:  3; ED: 13; DayOffset: lotDecOne),
    (SY:  834; SM:  5; SD: 12; EY:  834; EM:  6; ED: 10; DayOffset: lotDecOne),
    (SY:  834; SM: 10; SD:  6; EY:  834; EM: 11; ED:  4; DayOffset: lotDecOne),
    (SY:  835; SM:  2; SD:  1; EY:  835; EM:  3; ED:  2; DayOffset: lotDecOne),
    (SY:  835; SM:  5; SD:  1; EY:  835; EM:  5; ED: 30; DayOffset: lotDecOne),
    (SY:  835; SM: 10; SD: 25; EY:  835; EM: 11; ED: 23; DayOffset: lotDecOne),
    (SY:  835; SM: 12; SD: 23; EY:  836; EM:  1; ED: 21; DayOffset: lotDecOne),
    (SY:  836; SM:  2; SD: 20; EY:  836; EM:  3; ED: 20; DayOffset: lotDecOne),
    (SY:  837; SM:  1; SD: 10; EY:  837; EM:  2; ED:  8; DayOffset: lotDecOne),
    (SY:  837; SM:  3; SD: 10; EY:  837; EM:  4; ED:  8; DayOffset: lotDecOne),
    (SY:  837; SM:  5; SD:  8; EY:  837; EM:  6; ED:  6; DayOffset: lotDecOne),
    (SY:  837; SM:  7; SD:  6; EY:  837; EM:  8; ED:  4; DayOffset: lotDecOne),
    (SY:  837; SM: 10; SD:  3; EY:  837; EM: 11; ED:  1; DayOffset: lotDecOne),
    (SY:  838; SM:  3; SD: 29; EY:  838; EM:  4; ED: 27; DayOffset: lotDecOne),
    (SY:  838; SM:  9; SD: 22; EY:  838; EM: 10; ED: 21; DayOffset: lotDecOne),
    (SY:  839; SM:  4; SD: 17; EY:  839; EM:  5; ED: 16; DayOffset: lotDecOne),
    (SY:  839; SM:  7; SD: 14; EY:  839; EM:  8; ED: 12; DayOffset: lotDecOne),
    (SY:  839; SM:  9; SD: 11; EY:  839; EM: 10; ED: 10; DayOffset: lotDecOne),
    (SY:  840; SM:  5; SD:  5; EY:  840; EM:  6; ED:  3; DayOffset: lotDecOne),
    (SY:  840; SM:  8; SD:  1; EY:  840; EM:  8; ED: 30; DayOffset: lotDecOne),
    (SY:  840; SM:  9; SD: 29; EY:  840; EM: 10; ED: 28; DayOffset: lotDecOne),
    (SY:  841; SM:  1; SD: 26; EY:  841; EM:  2; ED: 24; DayOffset: lotDecOne),
    (SY:  841; SM:  5; SD: 24; EY:  841; EM:  6; ED: 22; DayOffset: lotDecOne),
    (SY:  842; SM:  1; SD: 15; EY:  842; EM:  2; ED: 13; DayOffset: lotDecOne),
    (SY:  842; SM:  4; SD: 14; EY:  842; EM:  5; ED: 13; DayOffset: lotDecOne),
    (SY:  842; SM:  6; SD: 12; EY:  842; EM:  7; ED: 11; DayOffset: lotDecOne),
    (SY:  843; SM:  9; SD: 27; EY:  843; EM: 10; ED: 26; DayOffset: lotDecOne),
    (SY:  844; SM:  1; SD: 23; EY:  844; EM:  2; ED: 21; DayOffset: lotDecOne),
    (SY:  844; SM:  4; SD: 21; EY:  844; EM:  5; ED: 20; DayOffset: lotDecOne),
    (SY:  844; SM: 10; SD: 15; EY:  844; EM: 11; ED: 13; DayOffset: lotDecOne),
    (SY:  845; SM:  2; SD: 10; EY:  845; EM:  3; ED: 11; DayOffset: lotDecOne),
    (SY:  845; SM:  4; SD: 10; EY:  845; EM:  5; ED:  9; DayOffset: lotDecOne),
    (SY:  845; SM:  9; SD:  5; EY:  845; EM: 10; ED:  4; DayOffset: lotDecOne),
    (SY:  845; SM: 11; SD:  3; EY:  845; EM: 12; ED:  2; DayOffset: lotDecOne),
    (SY:  846; SM:  1; SD:  1; EY:  846; EM:  1; ED: 30; DayOffset: lotDecOne),
    (SY:  846; SM:  6; SD: 27; EY:  846; EM:  7; ED: 26; DayOffset: lotDecOne),
    (SY:  846; SM: 11; SD: 22; EY:  846; EM: 12; ED: 21; DayOffset: lotDecOne),
    (SY:  847; SM:  3; SD: 20; EY:  847; EM:  4; ED: 18; DayOffset: lotDecOne),
    (SY:  847; SM:  6; SD: 16; EY:  847; EM:  7; ED: 15; DayOffset: lotDecOne),
    (SY:  847; SM:  9; SD: 13; EY:  847; EM: 10; ED: 12; DayOffset: lotDecOne),
    (SY:  847; SM: 12; SD: 11; EY:  848; EM:  1; ED:  9; DayOffset: lotDecOne),
    (SY:  848; SM:  4; SD:  7; EY:  848; EM:  5; ED:  6; DayOffset: lotDecOne),
    (SY:  848; SM:  7; SD:  4; EY:  848; EM:  8; ED:  2; DayOffset: lotDecOne),
    (SY:  848; SM:  9; SD:  1; EY:  848; EM:  9; ED: 30; DayOffset: lotDecOne),
    (SY:  848; SM: 12; SD: 29; EY:  849; EM:  1; ED: 27; DayOffset: lotDecOne),
    (SY:  849; SM:  4; SD: 26; EY:  849; EM:  5; ED: 25; DayOffset: lotDecOne),
    (SY:  849; SM: 12; SD: 18; EY:  850; EM:  1; ED: 16; DayOffset: lotDecOne),
    (SY:  850; SM:  3; SD: 17; EY:  850; EM:  4; ED: 15; DayOffset: lotDecOne),
    (SY:  850; SM:  5; SD: 15; EY:  850; EM:  6; ED: 13; DayOffset: lotDecOne),
    (SY:  850; SM: 12; SD:  7; EY:  851; EM:  1; ED:  5; DayOffset: lotDecOne),
    (SY:  851; SM:  4; SD:  5; EY:  851; EM:  5; ED:  4; DayOffset: lotDecOne),
    (SY:  851; SM:  6; SD:  3; EY:  851; EM:  7; ED:  2; DayOffset: lotDecOne),
    (SY:  852; SM:  3; SD: 24; EY:  852; EM:  4; ED: 22; DayOffset: lotDecOne),
    (SY:  853; SM:  3; SD: 13; EY:  853; EM:  4; ED: 11; DayOffset: lotDecOne),
    (SY:  853; SM:  6; SD: 10; EY:  853; EM:  7; ED:  9; DayOffset: lotDecOne),
    (SY:  853; SM:  8; SD:  8; EY:  853; EM:  9; ED:  6; DayOffset: lotDecOne),
    (SY:  853; SM: 10; SD:  6; EY:  853; EM: 11; ED:  4; DayOffset: lotDecOne),
    (SY:  854; SM:  5; SD: 30; EY:  854; EM:  6; ED: 28; DayOffset: lotDecOne),
    (SY:  854; SM: 10; SD: 25; EY:  854; EM: 11; ED: 23; DayOffset: lotDecOne),
    (SY:  854; SM: 12; SD: 23; EY:  855; EM:  1; ED: 21; DayOffset: lotDecOne),
    (SY:  855; SM:  8; SD: 16; EY:  855; EM:  9; ED: 14; DayOffset: lotDecOne),
    (SY:  855; SM: 11; SD: 13; EY:  855; EM: 12; ED: 12; DayOffset: lotDecOne),
    (SY:  856; SM:  1; SD: 11; EY:  856; EM:  2; ED:  9; DayOffset: lotDecOne),
    (SY:  856; SM:  6; SD:  6; EY:  856; EM:  7; ED:  5; DayOffset: lotDecOne),
    (SY:  856; SM:  8; SD:  4; EY:  856; EM:  9; ED:  2; DayOffset: lotDecOne),
    (SY:  857; SM:  3; SD: 29; EY:  857; EM:  4; ED: 27; DayOffset: lotDecOne),
    (SY:  857; SM:  6; SD: 25; EY:  857; EM:  7; ED: 24; DayOffset: lotDecOne),
    (SY:  857; SM: 11; SD: 20; EY:  857; EM: 12; ED: 19; DayOffset: lotDecOne),
    (SY:  858; SM:  2; SD: 17; EY:  858; EM:  3; ED: 18; DayOffset: lotDecOne),
    (SY:  858; SM:  4; SD: 17; EY:  858; EM:  5; ED: 16; DayOffset: lotDecOne),
    (SY:  858; SM:  7; SD: 14; EY:  858; EM:  8; ED: 12; DayOffset: lotDecOne),
    (SY:  858; SM: 11; SD:  9; EY:  858; EM: 12; ED:  8; DayOffset: lotDecOne),
    (SY:  859; SM:  3; SD:  8; EY:  859; EM:  4; ED:  6; DayOffset: lotDecOne),
    (SY:  859; SM:  5; SD:  6; EY:  859; EM:  6; ED:  4; DayOffset: lotDecOne),
    (SY:  860; SM:  2; SD: 25; EY:  860; EM:  3; ED: 25; DayOffset: lotDecOne),
    (SY:  860; SM:  8; SD: 20; EY:  860; EM:  9; ED: 18; DayOffset: lotDecOne),
    (SY:  861; SM:  2; SD: 13; EY:  861; EM:  3; ED: 14; DayOffset: lotDecOne),
    (SY:  861; SM:  9; SD:  8; EY:  861; EM: 10; ED:  7; DayOffset: lotDecOne),
    (SY:  862; SM:  3; SD:  4; EY:  862; EM:  4; ED:  2; DayOffset: lotDecOne),
    (SY:  862; SM:  7; SD: 30; EY:  862; EM:  8; ED: 28; DayOffset: lotDecOne),
    (SY:  862; SM:  9; SD: 27; EY:  862; EM: 10; ED: 26; DayOffset: lotDecOne),
    (SY:  863; SM:  5; SD: 21; EY:  863; EM:  6; ED: 19; DayOffset: lotDecOne),
    (SY:  863; SM:  7; SD: 19; EY:  863; EM:  8; ED: 17; DayOffset: lotDecOne),
    (SY:  863; SM: 12; SD: 14; EY:  864; EM:  1; ED: 12; DayOffset: lotDecOne),
    (SY:  864; SM:  5; SD:  9; EY:  864; EM:  6; ED:  7; DayOffset: lotDecOne),
    (SY:  865; SM:  1; SD:  1; EY:  865; EM:  1; ED: 30; DayOffset: lotDecOne),
    (SY:  865; SM:  5; SD: 28; EY:  865; EM:  6; ED: 26; DayOffset: lotDecOne),
    (SY:  865; SM:  7; SD: 26; EY:  865; EM:  8; ED: 24; DayOffset: lotDecOne),
    (SY:  865; SM: 10; SD: 23; EY:  865; EM: 11; ED: 21; DayOffset: lotDecOne),
    (SY:  866; SM:  1; SD: 20; EY:  866; EM:  2; ED: 18; DayOffset: lotDecOne),
    (SY:  866; SM:  3; SD: 20; EY:  866; EM:  4; ED: 18; DayOffset: lotDecOne),
    (SY:  866; SM: 10; SD: 12; EY:  866; EM: 11; ED: 10; DayOffset: lotDecOne),
    (SY:  867; SM:  2; SD:  8; EY:  867; EM:  3; ED:  9; DayOffset: lotDecOne),
    (SY:  867; SM:  4; SD:  8; EY:  867; EM:  5; ED:  7; DayOffset: lotDecOne),
    (SY:  867; SM: 10; SD:  1; EY:  867; EM: 10; ED: 30; DayOffset: lotDecOne),
    (SY:  868; SM:  4; SD: 26; EY:  868; EM:  5; ED: 25; DayOffset: lotDecOne),
    (SY:  868; SM: 10; SD: 19; EY:  868; EM: 11; ED: 17; DayOffset: lotDecOne),
    (SY:  869; SM:  1; SD: 16; EY:  869; EM:  2; ED: 14; DayOffset: lotDecOne),
    (SY:  869; SM:  8; SD: 11; EY:  869; EM:  9; ED:  9; DayOffset: lotDecOne),
    (SY:  869; SM: 11; SD:  7; EY:  869; EM: 12; ED:  6; DayOffset: lotDecOne),
    (SY:  870; SM:  2; SD:  4; EY:  870; EM:  3; ED:  5; DayOffset: lotDecOne),
    (SY:  870; SM:  5; SD:  4; EY:  870; EM:  6; ED:  2; DayOffset: lotDecOne),
    (SY:  870; SM:  7; SD:  2; EY:  870; EM:  7; ED: 31; DayOffset: lotDecOne),
    (SY:  870; SM:  8; SD: 30; EY:  870; EM:  9; ED: 28; DayOffset: lotDecOne),
    (SY:  871; SM:  4; SD: 23; EY:  871; EM:  5; ED: 22; DayOffset: lotDecOne),
    (SY:  872; SM:  7; SD:  9; EY:  872; EM:  8; ED:  7; DayOffset: lotDecOne),
    (SY:  873; SM:  4; SD: 30; EY:  873; EM:  5; ED: 29; DayOffset: lotDecOne),
    (SY:  873; SM:  6; SD: 28; EY:  873; EM:  7; ED: 27; DayOffset: lotDecOne),
    (SY:  873; SM:  9; SD: 25; EY:  873; EM: 10; ED: 24; DayOffset: lotDecOne),
    (SY:  873; SM: 12; SD: 23; EY:  874; EM:  1; ED: 21; DayOffset: lotDecOne),
    (SY:  874; SM:  5; SD: 19; EY:  874; EM:  6; ED: 17; DayOffset: lotDecOne),
    (SY:  874; SM:  9; SD: 14; EY:  874; EM: 10; ED: 13; DayOffset: lotDecOne),
    (SY:  875; SM:  3; SD: 11; EY:  875; EM:  4; ED:  9; DayOffset: lotDecOne),
    (SY:  875; SM:  6; SD:  7; EY:  875; EM:  7; ED:  6; DayOffset: lotDecOne),
    (SY:  875; SM:  9; SD:  3; EY:  875; EM: 10; ED:  2; DayOffset: lotDecOne),
    (SY:  875; SM: 12; SD: 31; EY:  876; EM:  1; ED: 29; DayOffset: lotDecOne),
    (SY:  876; SM:  3; SD: 29; EY:  876; EM:  4; ED: 27; DayOffset: lotDecOne),
    (SY:  876; SM:  9; SD: 21; EY:  876; EM: 10; ED: 20; DayOffset: lotDecOne),
    (SY:  876; SM: 12; SD: 19; EY:  877; EM:  1; ED: 17; DayOffset: lotDecOne),
    (SY:  877; SM:  7; SD: 14; EY:  877; EM:  8; ED: 12; DayOffset: lotDecOne),
    (SY:  877; SM: 10; SD: 10; EY:  877; EM: 11; ED:  8; DayOffset: lotDecOne),
    (SY:  878; SM:  4; SD:  6; EY:  878; EM:  5; ED:  5; DayOffset: lotDecOne),
    (SY:  878; SM:  8; SD:  2; EY:  878; EM:  8; ED: 31; DayOffset: lotDecOne),
    (SY:  878; SM: 10; SD: 29; EY:  878; EM: 11; ED: 27; DayOffset: lotDecOne),
    (SY:  878; SM: 12; SD: 27; EY:  879; EM:  1; ED: 25; DayOffset: lotDecOne),
    (SY:  879; SM:  3; SD: 26; EY:  879; EM:  4; ED: 24; DayOffset: lotDecOne),
    (SY:  879; SM:  6; SD: 23; EY:  879; EM:  7; ED: 22; DayOffset: lotDecOne),
    (SY:  879; SM: 11; SD: 17; EY:  879; EM: 12; ED: 16; DayOffset: lotDecOne),
    (SY:  880; SM:  1; SD: 15; EY:  880; EM:  2; ED: 13; DayOffset: lotDecOne),
    (SY:  880; SM:  6; SD: 11; EY:  880; EM:  7; ED: 10; DayOffset: lotDecOne),
    (SY:  881; SM:  2; SD:  2; EY:  881; EM:  3; ED:  3; DayOffset: lotDecOne),
    (SY:  881; SM:  4; SD:  2; EY:  881; EM:  5; ED:  1; DayOffset: lotDecOne),
    (SY:  881; SM:  8; SD: 28; EY:  881; EM:  9; ED: 26; DayOffset: lotDecOne),
    (SY:  882; SM:  4; SD: 21; EY:  882; EM:  5; ED: 20; DayOffset: lotDecOne),
    (SY:  882; SM:  6; SD: 19; EY:  882; EM:  7; ED: 18; DayOffset: lotDecOne),
    (SY:  882; SM:  8; SD: 17; EY:  882; EM:  9; ED: 15; DayOffset: lotDecOne),
    (SY:  883; SM:  5; SD: 10; EY:  883; EM:  6; ED:  8; DayOffset: lotDecOne),
    (SY:  883; SM: 12; SD:  3; EY:  884; EM:  1; ED:  1; DayOffset: lotDecOne),
    (SY:  884; SM:  5; SD: 28; EY:  884; EM:  6; ED: 26; DayOffset: lotDecOne),
    (SY:  884; SM:  8; SD: 24; EY:  884; EM:  9; ED: 22; DayOffset: lotDecOne),
    (SY:  884; SM: 11; SD: 21; EY:  884; EM: 12; ED: 20; DayOffset: lotDecOne),
    (SY:  885; SM:  6; SD: 16; EY:  885; EM:  7; ED: 15; DayOffset: lotDecOne),
    (SY:  885; SM:  9; SD: 12; EY:  885; EM: 10; ED: 11; DayOffset: lotDecOne),
    (SY:  885; SM: 11; SD: 10; EY:  885; EM: 12; ED:  9; DayOffset: lotDecOne),
    (SY:  886; SM:  7; SD:  5; EY:  886; EM:  8; ED:  3; DayOffset: lotDecOne),
    (SY:  886; SM: 10; SD:  1; EY:  886; EM: 10; ED: 30; DayOffset: lotDecOne),
    (SY:  886; SM: 11; SD: 29; EY:  886; EM: 12; ED: 28; DayOffset: lotDecOne),
    (SY:  887; SM:  3; SD: 28; EY:  887; EM:  4; ED: 26; DayOffset: lotDecOne),
    (SY:  887; SM:  5; SD: 26; EY:  887; EM:  6; ED: 24; DayOffset: lotDecOne),
    (SY:  887; SM: 10; SD: 20; EY:  887; EM: 11; ED: 18; DayOffset: lotDecOne),
    (SY:  887; SM: 12; SD: 18; EY:  888; EM:  1; ED: 16; DayOffset: lotDecOne),
    (SY:  888; SM:  3; SD: 16; EY:  888; EM:  4; ED: 14; DayOffset: lotDecOne),
    (SY:  888; SM: 11; SD:  7; EY:  888; EM: 12; ED:  6; DayOffset: lotDecOne),
    (SY:  889; SM:  1; SD:  5; EY:  889; EM:  2; ED:  3; DayOffset: lotDecOne),
    (SY:  889; SM:  3; SD:  5; EY:  889; EM:  4; ED:  3; DayOffset: lotDecOne),
    (SY:  889; SM:  6; SD:  2; EY:  889; EM:  7; ED:  1; DayOffset: lotDecOne),
    (SY:  890; SM:  1; SD: 24; EY:  890; EM:  2; ED: 22; DayOffset: lotDecOne),
    (SY:  890; SM:  3; SD: 24; EY:  890; EM:  4; ED: 22; DayOffset: lotDecOne),
    (SY:  890; SM:  5; SD: 22; EY:  890; EM:  6; ED: 20; DayOffset: lotDecOne),
    (SY:  891; SM:  4; SD: 12; EY:  891; EM:  5; ED: 11; DayOffset: lotDecOne),
    (SY:  891; SM: 11; SD:  5; EY:  891; EM: 12; ED:  4; DayOffset: lotDecOne),
    (SY:  892; SM:  7; SD: 27; EY:  892; EM:  8; ED: 25; DayOffset: lotDecOne),
    (SY:  892; SM: 10; SD: 24; EY:  892; EM: 11; ED: 22; DayOffset: lotDecOne),
    (SY:  893; SM:  5; SD: 19; EY:  893; EM:  6; ED: 17; DayOffset: lotDecOne),
    (SY:  893; SM:  8; SD: 15; EY:  893; EM:  9; ED: 13; DayOffset: lotDecOne),
    (SY:  893; SM: 10; SD: 13; EY:  893; EM: 11; ED: 11; DayOffset: lotDecOne),
    (SY:  894; SM: 11; SD:  1; EY:  894; EM: 11; ED: 30; DayOffset: lotDecOne),
    (SY:  895; SM:  2; SD: 28; EY:  895; EM:  3; ED: 29; DayOffset: lotDecOne),
    (SY:  895; SM:  6; SD: 26; EY:  895; EM:  7; ED: 25; DayOffset: lotDecOne),
    (SY:  896; SM:  2; SD: 17; EY:  896; EM:  3; ED: 17; DayOffset: lotDecOne),
    (SY:  896; SM:  5; SD: 16; EY:  896; EM:  6; ED: 14; DayOffset: lotDecOne),
    (SY:  896; SM: 10; SD: 10; EY:  896; EM: 11; ED:  8; DayOffset: lotDecOne),
    (SY:  897; SM:  2; SD:  5; EY:  897; EM:  3; ED:  6; DayOffset: lotDecOne),
    (SY:  897; SM:  5; SD:  5; EY:  897; EM:  6; ED:  3; DayOffset: lotDecOne),
    (SY:  897; SM: 10; SD: 29; EY:  897; EM: 11; ED: 27; DayOffset: lotDecOne),
    (SY:  897; SM: 12; SD: 27; EY:  898; EM:  1; ED: 25; DayOffset: lotDecOne),
    (SY:  898; SM:  2; SD: 24; EY:  898; EM:  3; ED: 25; DayOffset: lotDecOne),
    (SY:  898; SM:  4; SD: 24; EY:  898; EM:  5; ED: 23; DayOffset: lotDecOne),
    (SY:  898; SM:  7; SD: 22; EY:  898; EM:  8; ED: 20; DayOffset: lotDecOne),
    (SY:  898; SM:  9; SD: 19; EY:  898; EM: 10; ED: 18; DayOffset: lotDecOne),
    (SY:  899; SM:  1; SD: 15; EY:  899; EM:  2; ED: 13; DayOffset: lotDecOne),
    (SY:  899; SM:  5; SD: 13; EY:  899; EM:  6; ED: 11; DayOffset: lotDecOne),
    (SY:  899; SM:  7; SD: 11; EY:  899; EM:  8; ED:  9; DayOffset: lotDecOne),
    (SY:  899; SM: 10; SD:  8; EY:  899; EM: 11; ED:  6; DayOffset: lotDecOne),
    (SY:  900; SM:  2; SD:  3; EY:  900; EM:  3; ED:  3; DayOffset: lotDecOne),
    (SY:  900; SM:  9; SD: 26; EY:  900; EM: 10; ED: 25; DayOffset: lotDecOne),
    (SY:  901; SM:  4; SD: 21; EY:  901; EM:  5; ED: 20; DayOffset: lotDecOne),
    (SY:  901; SM:  7; SD: 18; EY:  901; EM:  8; ED: 16; DayOffset: lotDecOne),
    (SY:  901; SM:  9; SD: 15; EY:  901; EM: 10; ED: 14; DayOffset: lotDecOne),
    (SY:  902; SM:  5; SD: 10; EY:  902; EM:  6; ED:  8; DayOffset: lotDecOne),
    (SY:  902; SM: 10; SD:  4; EY:  902; EM: 11; ED:  2; DayOffset: lotDecOne),
    (SY:  903; SM:  1; SD: 31; EY:  903; EM:  3; ED:  1; DayOffset: lotDecOne),
    (SY:  903; SM:  5; SD: 29; EY:  903; EM:  6; ED: 27; DayOffset: lotDecOne),
    (SY:  904; SM:  1; SD: 20; EY:  904; EM:  2; ED: 18; DayOffset: lotDecOne),
    (SY:  904; SM:  4; SD: 18; EY:  904; EM:  5; ED: 17; DayOffset: lotDecOne),
    (SY:  904; SM:  6; SD: 16; EY:  904; EM:  7; ED: 15; DayOffset: lotDecOne),
    (SY:  905; SM:  1; SD:  8; EY:  905; EM:  2; ED:  6; DayOffset: lotDecOne),
    (SY:  905; SM: 10; SD:  1; EY:  905; EM: 10; ED: 30; DayOffset: lotDecOne),
    (SY:  906; SM:  1; SD: 27; EY:  906; EM:  2; ED: 25; DayOffset: lotDecOne),
    (SY:  906; SM:  6; SD: 24; EY:  906; EM:  7; ED: 23; DayOffset: lotDecOne),
    (SY:  906; SM:  8; SD: 22; EY:  906; EM:  9; ED: 20; DayOffset: lotDecOne),
    (SY:  906; SM: 10; SD: 20; EY:  906; EM: 11; ED: 18; DayOffset: lotDecOne),
    (SY:  907; SM:  4; SD: 15; EY:  907; EM:  5; ED: 14; DayOffset: lotDecOne),
    (SY:  907; SM:  9; SD: 10; EY:  907; EM: 10; ED:  9; DayOffset: lotDecOne),
    (SY:  907; SM: 11; SD:  8; EY:  907; EM: 12; ED:  7; DayOffset: lotDecOne),
    (SY:  908; SM:  1; SD:  6; EY:  908; EM:  2; ED:  4; DayOffset: lotDecOne),
    (SY:  908; SM: 11; SD: 26; EY:  908; EM: 12; ED: 25; DayOffset: lotDecOne),
    (SY:  909; SM:  1; SD: 24; EY:  909; EM:  2; ED: 22; DayOffset: lotDecOne),
    (SY:  909; SM:  6; SD: 20; EY:  909; EM:  7; ED: 19; DayOffset: lotDecOne),
    (SY:  909; SM: 12; SD: 15; EY:  910; EM:  1; ED: 13; DayOffset: lotDecOne),
    (SY:  910; SM:  4; SD: 12; EY:  910; EM:  5; ED: 11; DayOffset: lotDecOne),
    (SY:  910; SM:  7; SD:  9; EY:  910; EM:  8; ED:  7; DayOffset: lotDecOne),
    (SY:  910; SM:  9; SD:  6; EY:  910; EM: 10; ED:  5; DayOffset: lotDecOne),
    (SY:  911; SM:  1; SD:  3; EY:  911; EM:  2; ED:  1; DayOffset: lotDecOne),
    (SY:  911; SM:  5; SD:  1; EY:  911; EM:  5; ED: 30; DayOffset: lotDecOne),
    (SY:  911; SM: 12; SD: 23; EY:  912; EM:  1; ED: 21; DayOffset: lotDecOne),
    (SY:  912; SM:  3; SD: 21; EY:  912; EM:  4; ED: 19; DayOffset: lotDecOne),
    (SY:  912; SM:  5; SD: 19; EY:  912; EM:  6; ED: 17; DayOffset: lotDecOne),
    (SY:  912; SM: 12; SD: 11; EY:  913; EM:  1; ED:  9; DayOffset: lotDecOne),
    (SY:  913; SM:  4; SD:  9; EY:  913; EM:  5; ED:  8; DayOffset: lotDecOne),
    (SY:  914; SM:  3; SD: 29; EY:  914; EM:  4; ED: 27; DayOffset: lotDecOne),
    (SY:  914; SM:  9; SD: 22; EY:  914; EM: 10; ED: 21; DayOffset: lotDecOne),
    (SY:  915; SM:  3; SD: 18; EY:  915; EM:  4; ED: 16; DayOffset: lotDecOne),
    (SY:  915; SM:  6; SD: 15; EY:  915; EM:  7; ED: 14; DayOffset: lotDecOne),
    (SY:  915; SM:  8; SD: 13; EY:  915; EM:  9; ED: 11; DayOffset: lotDecOne),
    (SY:  915; SM: 10; SD: 11; EY:  915; EM: 11; ED:  9; DayOffset: lotDecOne),
    (SY:  916; SM:  6; SD:  3; EY:  916; EM:  7; ED:  2; DayOffset: lotDecOne),
    (SY:  916; SM: 10; SD: 29; EY:  916; EM: 11; ED: 27; DayOffset: lotDecOne),
    (SY:  916; SM: 12; SD: 27; EY:  917; EM:  1; ED: 25; DayOffset: lotDecOne),
    (SY:  917; SM:  8; SD: 20; EY:  917; EM:  9; ED: 18; DayOffset: lotDecOne),
    (SY:  917; SM: 11; SD: 17; EY:  917; EM: 12; ED: 16; DayOffset: lotDecOne),
    (SY:  918; SM:  1; SD: 15; EY:  918; EM:  2; ED: 13; DayOffset: lotDecOne),
    (SY:  918; SM:  6; SD: 11; EY:  918; EM:  7; ED: 10; DayOffset: lotDecOne),
    (SY:  918; SM:  8; SD:  9; EY:  918; EM:  9; ED:  7; DayOffset: lotDecOne),
    (SY:  919; SM:  2; SD:  3; EY:  919; EM:  3; ED:  4; DayOffset: lotDecOne),
    (SY:  919; SM:  4; SD:  3; EY:  919; EM:  5; ED:  2; DayOffset: lotDecOne),
    (SY:  919; SM: 11; SD: 25; EY:  919; EM: 12; ED: 24; DayOffset: lotDecOne),
    (SY:  920; SM:  2; SD: 22; EY:  920; EM:  3; ED: 22; DayOffset: lotDecOne),
    (SY:  920; SM:  4; SD: 21; EY:  920; EM:  5; ED: 20; DayOffset: lotDecOne),
    (SY:  920; SM: 11; SD: 13; EY:  920; EM: 12; ED: 12; DayOffset: lotDecOne),
    (SY:  921; SM:  3; SD: 12; EY:  921; EM:  4; ED: 10; DayOffset: lotDecOne),
    (SY:  921; SM:  5; SD: 10; EY:  921; EM:  6; ED:  8; DayOffset: lotDecOne),
    (SY:  921; SM: 11; SD:  2; EY:  921; EM: 12; ED:  1; DayOffset: lotDecOne),
    (SY:  922; SM:  3; SD:  1; EY:  922; EM:  3; ED: 30; DayOffset: lotDecOne),
    (SY:  922; SM: 11; SD: 21; EY:  922; EM: 12; ED: 20; DayOffset: lotDecOne),
    (SY:  923; SM:  2; SD: 18; EY:  923; EM:  3; ED: 19; DayOffset: lotDecOne),
    (SY:  923; SM:  5; SD: 18; EY:  923; EM:  6; ED: 16; DayOffset: lotDecOne),
    (SY:  923; SM:  7; SD: 16; EY:  923; EM:  8; ED: 14; DayOffset: lotDecOne),
    (SY:  924; SM:  8; SD:  3; EY:  924; EM:  9; ED:  1; DayOffset: lotDecOne),
    (SY:  924; SM: 10; SD:  1; EY:  924; EM: 10; ED: 30; DayOffset: lotDecOne),
    (SY:  925; SM:  7; SD: 23; EY:  925; EM:  8; ED: 21; DayOffset: lotDecOne),
    (SY:  925; SM: 12; SD: 18; EY:  926; EM:  1; ED: 16; DayOffset: lotDecOne),
    (SY:  926; SM:  5; SD: 14; EY:  926; EM:  6; ED: 12; DayOffset: lotDecOne),
    (SY:  926; SM:  7; SD: 12; EY:  926; EM:  8; ED: 10; DayOffset: lotDecOne),
    (SY:  927; SM:  1; SD:  6; EY:  927; EM:  2; ED:  4; DayOffset: lotDecOne),
    (SY:  927; SM:  6; SD:  2; EY:  927; EM:  7; ED:  1; DayOffset: lotDecOne),
    (SY:  927; SM:  7; SD: 31; EY:  927; EM:  8; ED: 29; DayOffset: lotDecOne),
    (SY:  927; SM: 10; SD: 28; EY:  927; EM: 11; ED: 26; DayOffset: lotDecOne),
    (SY:  928; SM:  1; SD: 25; EY:  928; EM:  2; ED: 23; DayOffset: lotDecOne),
    (SY:  928; SM:  3; SD: 24; EY:  928; EM:  4; ED: 22; DayOffset: lotDecOne),
    (SY:  928; SM: 10; SD: 16; EY:  928; EM: 11; ED: 14; DayOffset: lotDecOne),
    (SY:  929; SM:  2; SD: 12; EY:  929; EM:  3; ED: 13; DayOffset: lotDecOne),
    (SY:  929; SM:  4; SD: 12; EY:  929; EM:  5; ED: 11; DayOffset: lotDecOne),
    (SY:  929; SM: 10; SD:  5; EY:  929; EM: 11; ED:  3; DayOffset: lotDecOne),
    (SY:  930; SM:  2; SD:  1; EY:  930; EM:  3; ED:  2; DayOffset: lotDecOne),
    (SY:  930; SM: 10; SD: 24; EY:  930; EM: 11; ED: 22; DayOffset: lotDecOne),
    (SY:  931; SM:  1; SD: 21; EY:  931; EM:  2; ED: 19; DayOffset: lotDecOne),
    (SY:  931; SM: 11; SD: 12; EY:  931; EM: 12; ED: 11; DayOffset: lotDecOne),
    (SY:  932; SM:  5; SD:  8; EY:  932; EM:  6; ED:  6; DayOffset: lotDecOne),
    (SY:  932; SM:  7; SD:  6; EY:  932; EM:  8; ED:  4; DayOffset: lotDecOne),
    (SY:  932; SM: 11; SD: 30; EY:  932; EM: 12; ED: 29; DayOffset: lotDecOne),
    (SY:  933; SM:  4; SD: 27; EY:  933; EM:  5; ED: 26; DayOffset: lotDecOne),
    (SY:  934; SM:  2; SD: 16; EY:  934; EM:  3; ED: 17; DayOffset: lotDecOne),
    (SY:  934; SM:  4; SD: 16; EY:  934; EM:  5; ED: 15; DayOffset: lotDecOne),
    (SY:  934; SM:  7; SD: 14; EY:  934; EM:  8; ED: 12; DayOffset: lotDecOne),
    (SY:  935; SM:  5; SD:  5; EY:  935; EM:  6; ED:  3; DayOffset: lotDecOne),
    (SY:  935; SM:  7; SD:  3; EY:  935; EM:  8; ED:  1; DayOffset: lotDecOne),
    (SY:  935; SM:  9; SD: 30; EY:  935; EM: 10; ED: 29; DayOffset: lotDecOne),
    (SY:  935; SM: 12; SD: 28; EY:  936; EM:  1; ED: 26; DayOffset: lotDecOne),
    (SY:  936; SM:  5; SD: 23; EY:  936; EM:  6; ED: 21; DayOffset: lotDecOne),
    (SY:  936; SM:  9; SD: 18; EY:  936; EM: 10; ED: 17; DayOffset: lotDecOne),
    (SY:  937; SM:  2; SD: 13; EY:  937; EM:  3; ED: 14; DayOffset: lotIncOne),
    (SY:  937; SM:  9; SD:  7; EY:  937; EM: 10; ED:  6; DayOffset: lotDecOne),
    (SY:  938; SM:  2; SD:  2; EY:  938; EM:  4; ED:  2; DayOffset: lotIncOne),
    (SY:  938; SM:  7; SD: 29; EY:  938; EM:  8; ED: 27; DayOffset: lotDecOne),
    (SY:  938; SM:  9; SD: 26; EY:  938; EM: 10; ED: 25; DayOffset: lotDecOne),
    (SY:  938; SM: 12; SD: 24; EY:  939; EM:  1; ED: 22; DayOffset: lotDecOne),
    (SY:  939; SM: 10; SD: 15; EY:  939; EM: 11; ED: 13; DayOffset: lotDecOne),
    (SY:  939; SM: 12; SD: 13; EY:  940; EM:  1; ED: 11; DayOffset: lotDecOne),
    (SY:  940; SM:  4; SD: 10; EY:  940; EM:  5; ED:  9; DayOffset: lotDecOne),
    (SY:  940; SM:  6; SD:  8; EY:  940; EM:  7; ED:  7; DayOffset: lotDecOne),
    (SY:  940; SM: 11; SD:  2; EY:  940; EM: 12; ED:  1; DayOffset: lotDecOne),
    (SY:  940; SM: 12; SD: 31; EY:  941; EM:  1; ED: 29; DayOffset: lotDecOne),
    (SY:  941; SM:  3; SD: 30; EY:  941; EM:  4; ED: 28; DayOffset: lotDecOne),
    (SY:  941; SM:  6; SD: 27; EY:  941; EM:  7; ED: 26; DayOffset: lotDecOne),
    (SY:  941; SM: 11; SD: 21; EY:  941; EM: 12; ED: 20; DayOffset: lotDecOne),
    (SY:  942; SM:  1; SD: 19; EY:  942; EM:  2; ED: 17; DayOffset: lotDecOne),
    (SY:  942; SM:  6; SD: 16; EY:  942; EM:  7; ED: 15; DayOffset: lotDecOne),
    (SY:  943; SM:  2; SD:  7; EY:  943; EM:  3; ED:  8; DayOffset: lotDecOne),
    (SY:  943; SM:  4; SD:  7; EY:  943; EM:  5; ED:  6; DayOffset: lotDecOne),
    (SY:  943; SM:  6; SD:  5; EY:  943; EM:  7; ED:  4; DayOffset: lotDecOne),
    (SY:  943; SM:  9; SD:  2; EY:  943; EM: 10; ED:  1; DayOffset: lotDecOne),
    (SY:  944; SM:  2; SD: 26; EY:  944; EM:  3; ED: 26; DayOffset: lotDecOne),
    (SY:  944; SM:  4; SD: 25; EY:  944; EM:  5; ED: 24; DayOffset: lotDecOne),
    (SY:  944; SM:  8; SD: 21; EY:  944; EM:  9; ED: 19; DayOffset: lotDecOne),
    (SY:  945; SM:  8; SD: 10; EY:  945; EM:  9; ED:  8; DayOffset: lotDecOne),
    (SY:  945; SM: 12; SD:  7; EY:  946; EM:  1; ED:  5; DayOffset: lotDecOne),
    (SY:  946; SM:  7; SD:  1; EY:  946; EM:  7; ED: 30; DayOffset: lotDecOne),
    (SY:  946; SM:  8; SD: 29; EY:  946; EM:  9; ED: 27; DayOffset: lotDecOne),
    (SY:  946; SM: 11; SD: 26; EY:  946; EM: 12; ED: 25; DayOffset: lotDecOne),
    (SY:  947; SM:  7; SD: 20; EY:  947; EM:  8; ED: 18; DayOffset: lotDecOne),
    (SY:  947; SM: 11; SD: 15; EY:  947; EM: 12; ED: 14; DayOffset: lotDecOne),
    (SY:  948; SM: 12; SD:  3; EY:  949; EM:  1; ED:  1; DayOffset: lotDecOne),
    (SY:  949; SM:  5; SD: 30; EY:  949; EM:  6; ED: 28; DayOffset: lotDecOne),
    (SY:  949; SM: 12; SD: 22; EY:  950; EM:  1; ED: 20; DayOffset: lotDecOne),
    (SY:  950; SM: 11; SD: 12; EY:  950; EM: 12; ED: 11; DayOffset: lotDecOne),
    (SY:  951; SM:  1; SD: 10; EY:  951; EM:  2; ED:  8; DayOffset: lotDecOne),
    (SY:  951; SM:  3; SD: 10; EY:  951; EM:  4; ED:  8; DayOffset: lotDecOne),
    (SY:  951; SM:  6; SD:  7; EY:  951; EM:  7; ED:  6; DayOffset: lotDecOne),
    (SY:  951; SM:  8; SD:  5; EY:  951; EM:  9; ED:  3; DayOffset: lotDecOne),
    (SY:  952; SM:  1; SD: 29; EY:  952; EM:  2; ED: 27; DayOffset: lotDecOne),
    (SY:  952; SM:  3; SD: 28; EY:  952; EM:  4; ED: 26; DayOffset: lotDecOne),
    (SY:  952; SM:  5; SD: 26; EY:  952; EM:  6; ED: 24; DayOffset: lotDecOne),
    (SY:  953; SM:  2; SD: 16; EY:  953; EM:  3; ED: 17; DayOffset: lotDecOne),
    (SY:  953; SM: 11; SD:  9; EY:  953; EM: 12; ED:  8; DayOffset: lotDecOne),
    (SY:  954; SM:  8; SD:  1; EY:  954; EM:  8; ED: 30; DayOffset: lotDecOne),
    (SY:  954; SM: 10; SD: 29; EY:  954; EM: 11; ED: 27; DayOffset: lotDecOne),
    (SY:  955; SM:  6; SD: 22; EY:  955; EM:  7; ED: 21; DayOffset: lotDecOne),
    (SY:  955; SM:  8; SD: 20; EY:  955; EM:  9; ED: 18; DayOffset: lotDecOne),
    (SY:  955; SM: 10; SD: 18; EY:  955; EM: 11; ED: 16; DayOffset: lotDecOne),
    (SY:  956; SM:  7; SD: 10; EY:  956; EM:  8; ED:  8; DayOffset: lotDecOne),
    (SY:  956; SM: 11; SD:  5; EY:  956; EM: 12; ED:  4; DayOffset: lotDecOne),
    (SY:  957; SM:  3; SD:  4; EY:  957; EM:  4; ED:  2; DayOffset: lotDecOne),
    (SY:  957; SM:  5; SD:  2; EY:  957; EM:  5; ED: 31; DayOffset: lotDecOne),
    (SY:  958; SM:  2; SD: 21; EY:  958; EM:  3; ED: 22; DayOffset: lotDecOne),
    (SY:  958; SM: 10; SD: 15; EY:  958; EM: 11; ED: 13; DayOffset: lotDecOne),
    (SY:  959; SM:  2; SD: 10; EY:  959; EM:  3; ED: 11; DayOffset: lotDecOne),
    (SY:  959; SM:  5; SD: 10; EY:  959; EM:  6; ED:  8; DayOffset: lotDecOne),
    (SY:  959; SM: 11; SD:  3; EY:  959; EM: 12; ED:  2; DayOffset: lotDecOne),
    (SY:  960; SM:  1; SD:  1; EY:  960; EM:  1; ED: 30; DayOffset: lotDecOne),
    (SY:  960; SM:  2; SD: 29; EY:  960; EM:  3; ED: 29; DayOffset: lotDecOne),
    (SY:  960; SM:  4; SD: 28; EY:  960; EM:  5; ED: 27; DayOffset: lotDecOne),
    (SY:  960; SM:  7; SD: 26; EY:  960; EM:  8; ED: 24; DayOffset: lotDecOne),
    (SY:  960; SM:  9; SD: 23; EY:  960; EM: 10; ED: 22; DayOffset: lotDecOne),
    (SY:  960; SM: 11; SD: 21; EY:  960; EM: 12; ED: 20; DayOffset: lotDecOne),
    (SY:  961; SM:  1; SD: 19; EY:  961; EM:  2; ED: 17; DayOffset: lotDecOne),
    (SY:  961; SM:  3; SD: 19; EY:  961; EM:  4; ED: 17; DayOffset: lotDecOne),
    (SY:  961; SM:  7; SD: 15; EY:  961; EM:  8; ED: 13; DayOffset: lotDecOne),
    (SY:  961; SM: 10; SD: 12; EY:  961; EM: 11; ED: 10; DayOffset: lotDecOne),
    (SY:  962; SM:  2; SD:  7; EY:  962; EM:  3; ED:  8; DayOffset: lotDecOne),
    (SY:  962; SM:  7; SD:  4; EY:  962; EM:  8; ED:  2; DayOffset: lotDecOne),
    (SY:  962; SM: 10; SD:  1; EY:  962; EM: 10; ED: 30; DayOffset: lotDecOne),
    (SY:  963; SM:  7; SD: 23; EY:  963; EM:  8; ED: 21; DayOffset: lotDecOne),
    (SY:  963; SM:  9; SD: 20; EY:  963; EM: 10; ED: 19; DayOffset: lotDecOne),
    (SY:  964; SM:  1; SD: 17; EY:  964; EM:  2; ED: 15; DayOffset: lotDecOne),
    (SY:  964; SM:  5; SD: 14; EY:  964; EM:  6; ED: 12; DayOffset: lotDecOne),
    (SY:  964; SM: 10; SD:  8; EY:  964; EM: 11; ED:  6; DayOffset: lotDecOne),
    (SY:  965; SM:  2; SD:  4; EY:  965; EM:  3; ED:  5; DayOffset: lotDecOne),
    (SY:  965; SM:  6; SD:  2; EY:  965; EM:  7; ED:  1; DayOffset: lotDecOne),
    (SY:  966; SM:  1; SD: 24; EY:  966; EM:  2; ED: 22; DayOffset: lotDecOne),
    (SY:  966; SM:  4; SD: 23; EY:  966; EM:  5; ED: 22; DayOffset: lotDecOne),
    (SY:  967; SM:  1; SD: 13; EY:  967; EM:  2; ED: 11; DayOffset: lotDecOne),
    (SY:  968; SM:  2; SD:  1; EY:  968; EM:  3; ED:  1; DayOffset: lotDecOne),
    (SY:  968; SM:  6; SD: 28; EY:  968; EM:  7; ED: 27; DayOffset: lotDecOne),
    (SY:  968; SM:  8; SD: 26; EY:  968; EM:  9; ED: 24; DayOffset: lotDecOne),
    (SY:  968; SM: 10; SD: 24; EY:  968; EM: 11; ED: 22; DayOffset: lotDecOne),
    (SY:  969; SM:  4; SD: 19; EY:  969; EM:  5; ED: 18; DayOffset: lotDecOne),
    (SY:  969; SM:  9; SD: 14; EY:  969; EM: 10; ED: 13; DayOffset: lotDecOne),
    (SY:  969; SM: 11; SD: 12; EY:  969; EM: 12; ED: 11; DayOffset: lotDecOne),
    (SY:  970; SM:  1; SD: 10; EY:  970; EM:  2; ED:  8; DayOffset: lotDecOne),
    (SY:  970; SM: 12; SD:  1; EY:  970; EM: 12; ED: 30; DayOffset: lotDecOne),
    (SY:  971; SM:  1; SD: 29; EY:  971; EM:  2; ED: 27; DayOffset: lotDecOne),
    (SY:  971; SM:  6; SD: 25; EY:  971; EM:  7; ED: 24; DayOffset: lotDecOne),
    (SY:  971; SM: 12; SD: 20; EY:  972; EM:  1; ED: 18; DayOffset: lotDecOne),
    (SY:  972; SM:  2; SD: 17; EY:  972; EM:  3; ED: 17; DayOffset: lotDecOne),
    (SY:  972; SM:  9; SD: 10; EY:  972; EM: 10; ED:  9; DayOffset: lotDecOne),
    (SY:  973; SM:  1; SD:  7; EY:  973; EM:  2; ED:  5; DayOffset: lotDecOne),
    (SY:  973; SM:  3; SD:  7; EY:  973; EM:  4; ED:  5; DayOffset: lotDecOne),
    (SY:  973; SM:  5; SD:  5; EY:  973; EM:  6; ED:  3; DayOffset: lotDecOne),
    (SY:  973; SM: 12; SD: 27; EY:  974; EM:  1; ED: 25; DayOffset: lotDecOne),
    (SY:  974; SM:  3; SD: 26; EY:  974; EM:  4; ED: 24; DayOffset: lotDecOne),
    (SY:  974; SM: 12; SD: 16; EY:  975; EM:  1; ED: 14; DayOffset: lotDecOne),
    (SY:  976; SM:  4; SD:  2; EY:  976; EM:  5; ED:  1; DayOffset: lotDecOne),
    (SY:  976; SM:  7; SD: 29; EY:  976; EM:  8; ED: 27; DayOffset: lotDecOne),
    (SY:  977; SM:  3; SD: 22; EY:  977; EM:  4; ED: 20; DayOffset: lotDecOne),
    (SY:  977; SM:  6; SD: 19; EY:  977; EM:  7; ED: 18; DayOffset: lotDecOne),
    (SY:  977; SM:  8; SD: 17; EY:  977; EM:  9; ED: 15; DayOffset: lotDecOne),
    (SY:  977; SM: 10; SD: 15; EY:  977; EM: 11; ED: 13; DayOffset: lotDecOne),
    (SY:  978; SM:  6; SD:  8; EY:  978; EM:  7; ED:  7; DayOffset: lotDecOne),
    (SY:  978; SM: 11; SD:  3; EY:  978; EM: 12; ED:  2; DayOffset: lotDecOne),
    (SY:  979; SM:  1; SD:  1; EY:  979; EM:  1; ED: 30; DayOffset: lotDecOne),
    (SY:  979; SM:  5; SD: 28; EY:  979; EM:  6; ED: 26; DayOffset: lotDecOne),
    (SY:  979; SM:  8; SD: 25; EY:  979; EM:  9; ED: 23; DayOffset: lotDecOne),
    (SY:  979; SM: 11; SD: 22; EY:  979; EM: 12; ED: 21; DayOffset: lotDecOne),
    (SY:  980; SM:  1; SD: 20; EY:  980; EM:  2; ED: 18; DayOffset: lotDecOne),
    (SY:  980; SM:  6; SD: 15; EY:  980; EM:  7; ED: 14; DayOffset: lotDecOne),
    (SY:  980; SM:  8; SD: 13; EY:  980; EM:  9; ED: 11; DayOffset: lotDecOne),
    (SY:  981; SM:  2; SD:  7; EY:  981; EM:  3; ED:  8; DayOffset: lotDecOne),
    (SY:  981; SM: 11; SD: 29; EY:  981; EM: 12; ED: 28; DayOffset: lotDecOne),
    (SY:  982; SM:  2; SD: 26; EY:  982; EM:  3; ED: 27; DayOffset: lotDecOne),
    (SY:  982; SM:  8; SD: 21; EY:  982; EM:  9; ED: 19; DayOffset: lotDecOne),
    (SY:  982; SM: 11; SD: 18; EY:  982; EM: 12; ED: 17; DayOffset: lotDecOne),
    (SY:  983; SM:  3; SD: 17; EY:  983; EM:  4; ED: 15; DayOffset: lotDecOne),
    (SY:  983; SM:  9; SD:  9; EY:  983; EM: 10; ED:  8; DayOffset: lotDecOne),
    (SY:  983; SM: 11; SD:  7; EY:  983; EM: 12; ED:  6; DayOffset: lotDecOne),
    (SY:  984; SM:  3; SD:  5; EY:  984; EM:  4; ED:  3; DayOffset: lotDecOne),
    (SY:  984; SM:  7; SD:  1; EY:  984; EM:  7; ED: 30; DayOffset: lotDecOne),
    (SY:  984; SM: 11; SD: 25; EY:  984; EM: 12; ED: 24; DayOffset: lotDecOne),
    (SY:  985; SM:  2; SD: 22; EY:  985; EM:  3; ED: 23; DayOffset: lotDecOne),
    (SY:  985; SM:  5; SD: 22; EY:  985; EM:  6; ED: 20; DayOffset: lotDecOne),
    (SY:  985; SM:  7; SD: 20; EY:  985; EM:  8; ED: 18; DayOffset: lotDecOne),
    (SY:  985; SM: 12; SD: 14; EY:  986; EM:  1; ED: 12; DayOffset: lotDecOne),
    (SY:  986; SM:  3; SD: 13; EY:  986; EM:  4; ED: 11; DayOffset: lotDecOne),
    (SY:  986; SM:  5; SD: 11; EY:  986; EM:  6; ED:  9; DayOffset: lotDecOne),
    (SY:  986; SM:  8; SD:  8; EY:  986; EM:  9; ED:  6; DayOffset: lotDecOne),
    (SY:  987; SM:  7; SD: 28; EY:  987; EM:  8; ED: 26; DayOffset: lotDecOne),
    (SY:  988; SM:  3; SD: 20; EY:  988; EM:  4; ED: 18; DayOffset: lotDecOne),
    (SY:  988; SM:  5; SD: 18; EY:  988; EM:  6; ED: 16; DayOffset: lotDecOne),
    (SY:  988; SM:  7; SD: 16; EY:  988; EM:  8; ED: 14; DayOffset: lotDecOne),
    (SY:  989; SM:  1; SD: 10; EY:  989; EM:  2; ED:  8; DayOffset: lotDecOne),
    (SY:  989; SM: 11; SD:  1; EY:  989; EM: 11; ED: 30; DayOffset: lotDecOne),
    (SY:  990; SM:  1; SD: 29; EY:  990; EM:  2; ED: 27; DayOffset: lotDecOne),
    (SY:  990; SM: 10; SD: 21; EY:  990; EM: 11; ED: 19; DayOffset: lotDecOne),
    (SY:  991; SM:  2; SD: 17; EY:  991; EM:  3; ED: 18; DayOffset: lotDecOne),
    (SY:  991; SM:  8; SD: 12; EY:  991; EM:  9; ED: 10; DayOffset: lotDecOne),
    (SY:  991; SM: 10; SD: 10; EY:  991; EM: 11; ED:  8; DayOffset: lotDecOne),
    (SY:  992; SM:  2; SD:  6; EY:  992; EM:  3; ED:  6; DayOffset: lotDecOne),
    (SY:  992; SM:  8; SD: 30; EY:  992; EM:  9; ED: 28; DayOffset: lotDecOne),
    (SY:  992; SM: 10; SD: 28; EY:  992; EM: 11; ED: 26; DayOffset: lotDecOne),
    (SY:  993; SM:  1; SD: 25; EY:  993; EM:  2; ED: 23; DayOffset: lotDecOne),
    (SY:  993; SM:  6; SD: 22; EY:  993; EM:  7; ED: 21; DayOffset: lotDecOne),
    (SY:  993; SM: 11; SD: 16; EY:  993; EM: 12; ED: 15; DayOffset: lotDecOne),
    (SY:  994; SM:  2; SD: 13; EY:  994; EM:  3; ED: 14; DayOffset: lotDecOne),
    (SY:  994; SM:  7; SD: 11; EY:  994; EM:  8; ED:  9; DayOffset: lotDecOne),
    (SY:  994; SM: 12; SD:  5; EY:  995; EM:  1; ED:  3; DayOffset: lotDecOne),
    (SY:  995; SM:  2; SD:  2; EY:  995; EM:  3; ED:  3; DayOffset: lotDecOne),
    (SY:  995; SM:  5; SD:  2; EY:  995; EM:  5; ED: 31; DayOffset: lotDecOne),
    (SY:  996; SM:  2; SD: 21; EY:  996; EM:  3; ED: 21; DayOffset: lotDecOne),
    (SY:  996; SM:  4; SD: 20; EY:  996; EM:  5; ED: 19; DayOffset: lotDecOne),
    (SY:  997; SM:  3; SD: 11; EY:  997; EM:  4; ED:  9; DayOffset: lotDecOne),
    (SY:  997; SM:  5; SD:  9; EY:  997; EM:  6; ED:  7; DayOffset: lotDecOne),
    (SY:  997; SM:  7; SD:  7; EY:  997; EM:  8; ED:  5; DayOffset: lotDecOne),
    (SY:  997; SM: 10; SD:  4; EY:  997; EM: 11; ED:  2; DayOffset: lotDecOne),
    (SY:  998; SM:  1; SD:  1; EY:  998; EM:  1; ED: 30; DayOffset: lotDecOne),
    (SY:  998; SM:  9; SD: 23; EY:  998; EM: 10; ED: 22; DayOffset: lotDecOne),
    (SY:  999; SM:  7; SD: 15; EY:  999; EM:  8; ED: 13; DayOffset: lotDecOne),
    (SY:  999; SM:  9; SD: 12; EY:  999; EM: 10; ED: 11; DayOffset: lotDecOne),
    (SY: 1000; SM:  1; SD:  9; EY: 1000; EM:  2; ED:  7; DayOffset: lotDecOne),
    (SY: 1000; SM:  8; SD:  2; EY: 1000; EM:  8; ED: 31; DayOffset: lotDecOne),
    (SY: 1000; SM:  9; SD: 30; EY: 1000; EM: 10; ED: 29; DayOffset: lotDecOne),

    (SY: 1000; SM:  1; SD:  9; EY: 1000; EM:  2; ED:  7; DayOffset: lotDecOne),
    (SY: 1000; SM:  8; SD:  2; EY: 1000; EM:  8; ED: 31; DayOffset: lotDecOne),
    (SY: 1000; SM:  9; SD: 30; EY: 1000; EM: 10; ED: 29; DayOffset: lotDecOne),
    (SY: 1000; SM: 12; SD: 28; EY: 1001; EM:  2; ED: 25; DayOffset: lotDecOne),
    (SY: 1001; SM:  8; SD: 21; EY: 1001; EM:  9; ED: 19; DayOffset: lotDecOne),
    (SY: 1001; SM: 10; SD: 19; EY: 1001; EM: 11; ED: 17; DayOffset: lotDecOne),
    (SY: 1001; SM: 12; SD: 17; EY: 1002; EM:  2; ED: 14; DayOffset: lotDecOne),
    (SY: 1002; SM:  4; SD: 15; EY: 1002; EM:  5; ED: 14; DayOffset: lotDecOne),
    (SY: 1002; SM:  6; SD: 13; EY: 1002; EM:  7; ED: 12; DayOffset: lotDecOne),
    (SY: 1002; SM: 11; SD:  7; EY: 1002; EM: 12; ED:  6; DayOffset: lotDecOne),
    (SY: 1003; SM:  1; SD:  5; EY: 1003; EM:  2; ED:  3; DayOffset: lotDecOne),
    (SY: 1003; SM:  4; SD:  4; EY: 1003; EM:  5; ED:  3; DayOffset: lotDecOne),
    (SY: 1003; SM: 11; SD: 26; EY: 1003; EM: 12; ED: 25; DayOffset: lotDecOne),
    (SY: 1004; SM:  1; SD: 24; EY: 1004; EM:  2; ED: 22; DayOffset: lotDecOne),
    (SY: 1004; SM:  6; SD: 20; EY: 1004; EM:  7; ED: 19; DayOffset: lotDecOne),
    (SY: 1005; SM:  2; SD: 11; EY: 1005; EM:  3; ED: 12; DayOffset: lotDecOne),
    (SY: 1005; SM:  4; SD: 11; EY: 1005; EM:  5; ED: 10; DayOffset: lotDecOne),
    (SY: 1005; SM:  6; SD:  9; EY: 1005; EM:  7; ED:  8; DayOffset: lotDecOne),
    (SY: 1005; SM:  9; SD:  6; EY: 1005; EM: 10; ED:  5; DayOffset: lotDecOne),
    (SY: 1006; SM:  3; SD:  2; EY: 1006; EM:  3; ED: 31; DayOffset: lotDecOne),
    (SY: 1006; SM:  8; SD: 26; EY: 1006; EM:  9; ED: 24; DayOffset: lotDecOne),
    (SY: 1007; SM:  3; SD: 21; EY: 1007; EM:  4; ED: 19; DayOffset: lotDecOne),
    (SY: 1007; SM:  8; SD: 15; EY: 1007; EM:  9; ED: 13; DayOffset: lotDecOne),
    (SY: 1007; SM: 12; SD: 12; EY: 1008; EM:  1; ED: 10; DayOffset: lotDecOne),
    (SY: 1008; SM:  7; SD:  5; EY: 1008; EM:  8; ED:  3; DayOffset: lotDecOne),
    (SY: 1008; SM:  9; SD:  2; EY: 1008; EM: 10; ED:  1; DayOffset: lotDecOne),
    (SY: 1008; SM: 11; SD: 30; EY: 1008; EM: 12; ED: 29; DayOffset: lotDecOne),
    (SY: 1009; SM:  7; SD: 24; EY: 1009; EM:  8; ED: 22; DayOffset: lotDecOne),
    (SY: 1009; SM: 11; SD: 19; EY: 1009; EM: 12; ED: 18; DayOffset: lotDecOne),
    (SY: 1010; SM:  3; SD: 18; EY: 1010; EM:  4; ED: 16; DayOffset: lotDecOne),
    (SY: 1010; SM:  5; SD: 16; EY: 1010; EM:  6; ED: 14; DayOffset: lotDecOne),
    (SY: 1010; SM:  8; SD: 12; EY: 1010; EM:  9; ED: 10; DayOffset: lotDecOne),
    (SY: 1010; SM: 12; SD:  8; EY: 1011; EM:  1; ED:  6; DayOffset: lotDecOne),
    (SY: 1011; SM:  6; SD:  4; EY: 1011; EM:  7; ED:  3; DayOffset: lotDecOne),
    (SY: 1011; SM:  8; SD: 31; EY: 1011; EM:  9; ED: 29; DayOffset: lotDecOne),
    (SY: 1011; SM: 12; SD: 27; EY: 1012; EM:  1; ED: 25; DayOffset: lotDecOne),
    (SY: 1012; SM: 11; SD: 16; EY: 1012; EM: 12; ED: 15; DayOffset: lotDecOne),
    (SY: 1013; SM:  1; SD: 14; EY: 1013; EM:  2; ED: 12; DayOffset: lotDecOne),
    (SY: 1013; SM:  3; SD: 14; EY: 1013; EM:  4; ED: 12; DayOffset: lotDecOne),
    (SY: 1013; SM:  8; SD:  9; EY: 1013; EM:  9; ED:  7; DayOffset: lotDecOne),
    (SY: 1013; SM: 12; SD:  5; EY: 1014; EM:  1; ED:  3; DayOffset: lotDecOne),
    (SY: 1014; SM:  2; SD:  2; EY: 1014; EM:  3; ED:  3; DayOffset: lotDecOne),
    (SY: 1014; SM:  5; SD: 31; EY: 1014; EM:  6; ED: 29; DayOffset: lotDecOne),
    (SY: 1014; SM: 10; SD: 26; EY: 1014; EM: 11; ED: 24; DayOffset: lotDecOne),
    (SY: 1015; SM:  2; SD: 21; EY: 1015; EM:  3; ED: 22; DayOffset: lotDecOne),
    (SY: 1015; SM: 11; SD: 14; EY: 1015; EM: 12; ED: 13; DayOffset: lotDecOne),
    (SY: 1016; SM:  3; SD: 11; EY: 1016; EM:  4; ED:  9; DayOffset: lotDecOne),
    (SY: 1016; SM:  6; SD:  7; EY: 1016; EM:  7; ED:  6; DayOffset: lotDecOne),
    (SY: 1016; SM:  8; SD:  5; EY: 1016; EM:  9; ED:  3; DayOffset: lotDecOne),
    (SY: 1016; SM: 11; SD:  2; EY: 1016; EM: 12; ED:  1; DayOffset: lotDecOne),
    (SY: 1017; SM:  6; SD: 26; EY: 1017; EM:  7; ED: 25; DayOffset: lotDecOne),
    (SY: 1017; SM: 10; SD: 22; EY: 1017; EM: 11; ED: 20; DayOffset: lotDecOne),
    (SY: 1018; SM:  2; SD: 18; EY: 1018; EM:  3; ED: 19; DayOffset: lotDecOne),
    (SY: 1018; SM:  7; SD: 15; EY: 1018; EM:  8; ED: 13; DayOffset: lotDecOne),
    (SY: 1018; SM: 11; SD: 10; EY: 1018; EM: 12; ED:  9; DayOffset: lotDecOne),
    (SY: 1019; SM:  3; SD:  9; EY: 1019; EM:  4; ED:  7; DayOffset: lotDecOne),
    (SY: 1019; SM:  5; SD:  7; EY: 1019; EM:  6; ED:  5; DayOffset: lotDecOne),
    (SY: 1019; SM:  8; SD:  3; EY: 1019; EM:  9; ED:  1; DayOffset: lotDecOne),
    (SY: 1020; SM:  2; SD: 26; EY: 1020; EM:  3; ED: 26; DayOffset: lotDecOne),
    (SY: 1020; SM:  8; SD: 21; EY: 1020; EM:  9; ED: 19; DayOffset: lotDecOne),
    (SY: 1021; SM:  2; SD: 14; EY: 1021; EM:  3; ED: 15; DayOffset: lotDecOne),
    (SY: 1021; SM:  5; SD: 14; EY: 1021; EM:  6; ED: 12; DayOffset: lotDecOne),
    (SY: 1021; SM:  9; SD:  9; EY: 1021; EM: 10; ED:  8; DayOffset: lotDecOne),
    (SY: 1021; SM: 11; SD:  7; EY: 1021; EM: 12; ED:  6; DayOffset: lotDecOne),
    (SY: 1022; SM:  1; SD:  5; EY: 1022; EM:  2; ED:  3; DayOffset: lotDecOne),
    (SY: 1022; SM:  5; SD:  3; EY: 1022; EM:  6; ED:  1; DayOffset: lotDecOne),
    (SY: 1022; SM:  7; SD: 31; EY: 1022; EM:  8; ED: 29; DayOffset: lotDecOne),
    (SY: 1022; SM:  9; SD: 28; EY: 1022; EM: 10; ED: 27; DayOffset: lotDecOne),
    (SY: 1022; SM: 11; SD: 26; EY: 1022; EM: 12; ED: 25; DayOffset: lotDecOne),
    (SY: 1023; SM:  1; SD: 24; EY: 1023; EM:  2; ED: 22; DayOffset: lotDecOne),
    (SY: 1023; SM:  7; SD: 20; EY: 1023; EM:  8; ED: 18; DayOffset: lotDecOne),
    (SY: 1023; SM: 10; SD: 17; EY: 1023; EM: 11; ED: 15; DayOffset: lotDecOne),
    (SY: 1023; SM: 12; SD: 15; EY: 1024; EM:  1; ED: 13; DayOffset: lotDecOne),
    (SY: 1024; SM:  2; SD: 12; EY: 1024; EM:  3; ED: 12; DayOffset: lotDecOne),
    (SY: 1024; SM:  7; SD:  8; EY: 1024; EM:  8; ED:  6; DayOffset: lotDecOne),
    (SY: 1024; SM: 10; SD:  5; EY: 1024; EM: 11; ED:  3; DayOffset: lotDecOne),
    (SY: 1025; SM:  1; SD:  2; EY: 1025; EM:  1; ED: 31; DayOffset: lotIncOne),
    (SY: 1025; SM:  3; SD:  2; EY: 1025; EM:  3; ED: 31; DayOffset: lotDecOne),
    (SY: 1025; SM:  5; SD: 29; EY: 1025; EM:  6; ED: 27; DayOffset: lotDecOne),
    (SY: 1025; SM:  9; SD: 24; EY: 1025; EM: 10; ED: 23; DayOffset: lotDecOne),
    (SY: 1026; SM:  1; SD: 21; EY: 1026; EM:  2; ED: 19; DayOffset: lotDecOne),
    (SY: 1026; SM:  3; SD: 21; EY: 1026; EM:  4; ED: 19; DayOffset: lotDecOne),
    (SY: 1026; SM:  6; SD: 17; EY: 1026; EM:  7; ED: 16; DayOffset: lotDecOne),
    (SY: 1027; SM:  4; SD:  9; EY: 1027; EM:  5; ED:  8; DayOffset: lotDecOne),
    (SY: 1027; SM:  7; SD:  6; EY: 1027; EM:  8; ED:  4; DayOffset: lotDecOne),
    (SY: 1028; SM:  1; SD: 29; EY: 1028; EM:  2; ED: 27; DayOffset: lotDecOne),
    (SY: 1028; SM:  4; SD: 27; EY: 1028; EM:  5; ED: 26; DayOffset: lotDecOne),
    (SY: 1028; SM:  7; SD: 24; EY: 1028; EM:  8; ED: 22; DayOffset: lotDecOne),
    (SY: 1029; SM:  1; SD: 17; EY: 1029; EM:  2; ED: 15; DayOffset: lotDecOne),
    (SY: 1029; SM:  8; SD: 12; EY: 1029; EM:  9; ED: 10; DayOffset: lotDecOne),
    (SY: 1030; SM:  7; SD:  3; EY: 1030; EM:  8; ED:  1; DayOffset: lotDecOne),
    (SY: 1030; SM:  8; SD: 31; EY: 1030; EM:  9; ED: 29; DayOffset: lotDecOne),
    (SY: 1030; SM: 10; SD: 29; EY: 1030; EM: 11; ED: 27; DayOffset: lotDecOne),
    (SY: 1031; SM:  4; SD: 24; EY: 1031; EM:  5; ED: 23; DayOffset: lotDecOne),
    (SY: 1031; SM:  6; SD: 22; EY: 1031; EM:  7; ED: 21; DayOffset: lotDecOne),
    (SY: 1031; SM:  9; SD: 19; EY: 1031; EM: 10; ED: 18; DayOffset: lotDecOne),
    (SY: 1031; SM: 11; SD: 17; EY: 1031; EM: 12; ED: 16; DayOffset: lotDecOne),
    (SY: 1032; SM:  1; SD: 15; EY: 1032; EM:  2; ED: 13; DayOffset: lotDecOne),
    (SY: 1032; SM:  9; SD:  7; EY: 1032; EM: 10; ED:  6; DayOffset: lotDecOne),
    (SY: 1032; SM: 12; SD:  5; EY: 1033; EM:  1; ED:  3; DayOffset: lotDecOne),
    (SY: 1033; SM:  2; SD:  2; EY: 1033; EM:  3; ED:  3; DayOffset: lotDecOne),
    (SY: 1033; SM:  5; SD:  1; EY: 1033; EM:  5; ED: 30; DayOffset: lotDecOne),
    (SY: 1033; SM:  8; SD: 27; EY: 1033; EM:  9; ED: 25; DayOffset: lotDecOne),
    (SY: 1033; SM: 12; SD: 24; EY: 1034; EM:  1; ED: 22; DayOffset: lotDecOne),
    (SY: 1034; SM:  2; SD: 21; EY: 1034; EM:  3; ED: 22; DayOffset: lotDecOne),
    (SY: 1034; SM:  5; SD: 20; EY: 1034; EM:  6; ED: 18; DayOffset: lotDecOne),
    (SY: 1034; SM:  9; SD: 15; EY: 1034; EM: 10; ED: 14; DayOffset: lotDecOne),
    (SY: 1035; SM:  1; SD: 12; EY: 1035; EM:  2; ED: 10; DayOffset: lotDecOne),
    (SY: 1035; SM:  3; SD: 12; EY: 1035; EM:  4; ED: 10; DayOffset: lotDecOne),
    (SY: 1035; SM:  6; SD:  8; EY: 1035; EM:  7; ED:  7; DayOffset: lotDecOne),
    (SY: 1036; SM:  1; SD:  1; EY: 1036; EM:  1; ED: 30; DayOffset: lotDecOne),
    (SY: 1036; SM:  3; SD: 30; EY: 1036; EM:  4; ED: 28; DayOffset: lotDecOne),
    (SY: 1036; SM:  6; SD: 26; EY: 1036; EM:  7; ED: 25; DayOffset: lotDecOne),
    (SY: 1036; SM:  9; SD: 22; EY: 1036; EM: 10; ED: 21; DayOffset: lotDecOne),
    (SY: 1036; SM: 12; SD: 20; EY: 1037; EM:  1; ED: 18; DayOffset: lotDecOne),
    (SY: 1037; SM:  7; SD: 15; EY: 1037; EM:  8; ED: 13; DayOffset: lotDecOne),
    (SY: 1038; SM:  4; SD:  7; EY: 1038; EM:  5; ED:  6; DayOffset: lotDecOne),
    (SY: 1038; SM:  8; SD:  3; EY: 1038; EM:  9; ED:  1; DayOffset: lotDecOne),
    (SY: 1039; SM:  3; SD: 27; EY: 1039; EM:  4; ED: 25; DayOffset: lotDecOne),
    (SY: 1039; SM:  8; SD: 22; EY: 1039; EM:  9; ED: 20; DayOffset: lotDecOne),
    (SY: 1039; SM: 10; SD: 20; EY: 1039; EM: 11; ED: 18; DayOffset: lotDecOne),
    (SY: 1040; SM: 11; SD:  7; EY: 1040; EM: 12; ED:  6; DayOffset: lotDecOne),
    (SY: 1041; SM:  1; SD:  5; EY: 1041; EM:  2; ED:  3; DayOffset: lotDecOne),
    (SY: 1041; SM:  6; SD:  1; EY: 1041; EM:  6; ED: 30; DayOffset: lotDecOne),
    (SY: 1041; SM:  8; SD: 29; EY: 1041; EM:  9; ED: 27; DayOffset: lotDecOne),
    (SY: 1041; SM: 11; SD: 26; EY: 1041; EM: 12; ED: 25; DayOffset: lotDecOne),
    (SY: 1042; SM:  1; SD: 24; EY: 1042; EM:  2; ED: 22; DayOffset: lotDecOne),
    (SY: 1042; SM:  4; SD: 22; EY: 1042; EM:  5; ED: 21; DayOffset: lotDecOne),
    (SY: 1042; SM:  8; SD: 18; EY: 1042; EM:  9; ED: 16; DayOffset: lotDecOne),
    (SY: 1043; SM:  2; SD: 12; EY: 1043; EM:  3; ED: 13; DayOffset: lotDecOne),
    (SY: 1043; SM:  5; SD: 11; EY: 1043; EM:  6; ED:  9; DayOffset: lotDecOne),
    (SY: 1043; SM: 11; SD:  4; EY: 1044; EM:  1; ED:  2; DayOffset: lotDecOne),
    (SY: 1044; SM:  3; SD:  2; EY: 1044; EM:  3; ED: 31; DayOffset: lotDecOne),
    (SY: 1044; SM:  5; SD: 29; EY: 1044; EM:  6; ED: 27; DayOffset: lotDecOne),
    (SY: 1044; SM:  8; SD: 25; EY: 1044; EM:  9; ED: 23; DayOffset: lotDecOne),
    (SY: 1045; SM:  6; SD: 17; EY: 1045; EM:  7; ED: 16; DayOffset: lotDecOne),
    (SY: 1045; SM:  9; SD: 13; EY: 1045; EM: 10; ED: 12; DayOffset: lotDecOne),
    (SY: 1045; SM: 11; SD: 11; EY: 1045; EM: 12; ED: 10; DayOffset: lotDecOne),
    (SY: 1046; SM:  3; SD: 10; EY: 1046; EM:  4; ED:  8; DayOffset: lotDecOne),
    (SY: 1046; SM:  7; SD:  6; EY: 1046; EM:  8; ED:  4; DayOffset: lotDecOne),
    (SY: 1046; SM: 10; SD:  2; EY: 1046; EM: 10; ED: 31; DayOffset: lotDecOne),
    (SY: 1046; SM: 11; SD: 30; EY: 1046; EM: 12; ED: 29; DayOffset: lotDecOne),
    (SY: 1047; SM:  2; SD: 27; EY: 1047; EM:  3; ED: 28; DayOffset: lotDecOne),
    (SY: 1047; SM:  5; SD: 27; EY: 1047; EM:  6; ED: 25; DayOffset: lotDecOne),
    (SY: 1047; SM:  7; SD: 25; EY: 1047; EM:  8; ED: 23; DayOffset: lotDecOne),
    (SY: 1047; SM: 12; SD: 19; EY: 1048; EM:  1; ED: 17; DayOffset: lotDecOne),
    (SY: 1048; SM:  5; SD: 15; EY: 1048; EM:  6; ED: 13; DayOffset: lotDecOne),
    (SY: 1049; SM:  3; SD:  6; EY: 1049; EM:  4; ED:  4; DayOffset: lotDecOne),
    (SY: 1049; SM:  8; SD:  1; EY: 1049; EM:  8; ED: 30; DayOffset: lotDecOne),
    (SY: 1049; SM: 12; SD: 27; EY: 1050; EM:  1; ED: 25; DayOffset: lotDecOne),
    (SY: 1050; SM:  3; SD: 25; EY: 1050; EM:  4; ED: 23; DayOffset: lotDecOne),
    (SY: 1050; SM:  5; SD: 23; EY: 1050; EM:  6; ED: 21; DayOffset: lotDecOne),
    (SY: 1050; SM:  7; SD: 21; EY: 1050; EM:  8; ED: 19; DayOffset: lotDecOne),
    (SY: 1050; SM: 10; SD: 18; EY: 1050; EM: 11; ED: 16; DayOffset: lotDecOne),
    (SY: 1051; SM:  1; SD: 15; EY: 1051; EM:  2; ED: 13; DayOffset: lotDecOne),
    (SY: 1051; SM:  4; SD: 13; EY: 1051; EM:  5; ED: 12; DayOffset: lotDecOne),
    (SY: 1051; SM: 11; SD:  6; EY: 1051; EM: 12; ED:  5; DayOffset: lotDecOne),
    (SY: 1052; SM:  2; SD:  3; EY: 1052; EM:  3; ED:  3; DayOffset: lotDecOne),
    (SY: 1052; SM:  5; SD:  1; EY: 1052; EM:  5; ED: 30; DayOffset: lotDecOne),
    (SY: 1052; SM:  7; SD: 28; EY: 1052; EM:  8; ED: 26; DayOffset: lotDecOne),
    (SY: 1052; SM: 10; SD: 25; EY: 1052; EM: 11; ED: 23; DayOffset: lotDecOne),
    (SY: 1053; SM:  5; SD: 20; EY: 1053; EM:  6; ED: 18; DayOffset: lotDecOne),
    (SY: 1053; SM:  8; SD: 16; EY: 1053; EM:  9; ED: 14; DayOffset: lotDecOne),
    (SY: 1053; SM: 10; SD: 14; EY: 1053; EM: 11; ED: 12; DayOffset: lotDecOne),
    (SY: 1054; SM:  2; SD: 10; EY: 1054; EM:  3; ED: 11; DayOffset: lotDecOne),
    (SY: 1054; SM:  6; SD:  8; EY: 1054; EM:  7; ED:  7; DayOffset: lotDecOne),
    (SY: 1054; SM:  9; SD:  4; EY: 1054; EM: 10; ED:  3; DayOffset: lotDecOne),
    (SY: 1054; SM: 11; SD:  2; EY: 1054; EM: 12; ED:  1; DayOffset: lotDecOne),
    (SY: 1055; SM:  1; SD: 30; EY: 1055; EM:  2; ED: 28; DayOffset: lotDecOne),
    (SY: 1055; SM:  6; SD: 27; EY: 1055; EM:  7; ED: 26; DayOffset: lotDecOne),
    (SY: 1055; SM:  9; SD: 23; EY: 1055; EM: 10; ED: 22; DayOffset: lotDecOne),
    (SY: 1055; SM: 11; SD: 21; EY: 1055; EM: 12; ED: 20; DayOffset: lotDecOne),
    (SY: 1056; SM: 12; SD:  9; EY: 1057; EM:  1; ED:  7; DayOffset: lotDecOne),
    (SY: 1057; SM:  2; SD:  6; EY: 1057; EM:  3; ED:  7; DayOffset: lotDecOne),
    (SY: 1057; SM:  5; SD:  6; EY: 1057; EM:  6; ED:  4; DayOffset: lotDecOne),
    (SY: 1058; SM:  2; SD: 25; EY: 1058; EM:  3; ED: 26; DayOffset: lotDecOne),
    (SY: 1058; SM:  4; SD: 25; EY: 1058; EM:  5; ED: 24; DayOffset: lotDecOne),
    (SY: 1059; SM:  3; SD: 16; EY: 1059; EM:  4; ED: 14; DayOffset: lotDecOne),
    (SY: 1059; SM:  5; SD: 14; EY: 1059; EM:  6; ED: 12; DayOffset: lotDecOne),
    (SY: 1059; SM: 10; SD:  9; EY: 1059; EM: 11; ED:  7; DayOffset: lotDecOne),
    (SY: 1060; SM:  1; SD:  6; EY: 1060; EM:  2; ED:  4; DayOffset: lotDecOne),
    (SY: 1060; SM:  4; SD:  3; EY: 1060; EM:  5; ED:  2; DayOffset: lotDecOne),
    (SY: 1060; SM:  9; SD: 27; EY: 1060; EM: 10; ED: 26; DayOffset: lotDecOne),
    (SY: 1061; SM:  4; SD: 22; EY: 1061; EM:  5; ED: 21; DayOffset: lotDecOne),
    (SY: 1061; SM:  7; SD: 19; EY: 1061; EM:  8; ED: 17; DayOffset: lotDecOne),
    (SY: 1061; SM:  9; SD: 16; EY: 1061; EM: 10; ED: 15; DayOffset: lotDecOne),
    (SY: 1062; SM:  1; SD: 13; EY: 1062; EM:  2; ED: 11; DayOffset: lotDecOne),
    (SY: 1062; SM:  5; SD: 11; EY: 1062; EM:  6; ED:  9; DayOffset: lotDecOne),
    (SY: 1062; SM:  8; SD:  7; EY: 1062; EM:  9; ED:  5; DayOffset: lotDecOne),
    (SY: 1062; SM: 10; SD:  5; EY: 1062; EM: 11; ED:  3; DayOffset: lotDecOne),
    (SY: 1063; SM:  1; SD:  2; EY: 1063; EM:  1; ED: 31; DayOffset: lotDecOne),
    (SY: 1063; SM:  5; SD: 30; EY: 1063; EM:  6; ED: 28; DayOffset: lotDecOne),
    (SY: 1063; SM:  8; SD: 26; EY: 1063; EM:  9; ED: 24; DayOffset: lotDecOne),
    (SY: 1063; SM: 12; SD: 22; EY: 1064; EM:  1; ED: 20; DayOffset: lotDecOne),
    (SY: 1064; SM:  4; SD: 19; EY: 1064; EM:  5; ED: 18; DayOffset: lotDecOne),
    (SY: 1064; SM:  6; SD: 17; EY: 1064; EM:  7; ED: 16; DayOffset: lotDecOne),
    (SY: 1064; SM:  9; SD: 13; EY: 1064; EM: 10; ED: 12; DayOffset: lotDecOne),
    (SY: 1065; SM:  1; SD:  9; EY: 1065; EM:  2; ED:  7; DayOffset: lotDecOne),
    (SY: 1065; SM:  4; SD:  8; EY: 1065; EM:  5; ED:  7; DayOffset: lotDecOne),
    (SY: 1065; SM: 11; SD: 30; EY: 1065; EM: 12; ED: 29; DayOffset: lotDecOne),
    (SY: 1066; SM:  1; SD: 28; EY: 1066; EM:  2; ED: 26; DayOffset: lotDecOne),
    (SY: 1066; SM:  3; SD: 28; EY: 1066; EM:  4; ED: 26; DayOffset: lotDecOne),
    (SY: 1066; SM:  6; SD: 25; EY: 1066; EM:  7; ED: 24; DayOffset: lotDecOne),
    (SY: 1066; SM: 12; SD: 19; EY: 1067; EM:  1; ED: 17; DayOffset: lotDecOne),
    (SY: 1067; SM:  2; SD: 16; EY: 1067; EM:  3; ED: 17; DayOffset: lotDecOne),
    (SY: 1067; SM:  4; SD: 16; EY: 1067; EM:  5; ED: 15; DayOffset: lotDecOne),
    (SY: 1067; SM:  6; SD: 14; EY: 1067; EM:  7; ED: 13; DayOffset: lotDecOne),
    (SY: 1067; SM:  9; SD: 11; EY: 1067; EM: 10; ED: 10; DayOffset: lotDecOne),
    (SY: 1068; SM:  3; SD:  6; EY: 1068; EM:  4; ED:  4; DayOffset: lotDecOne),
    (SY: 1068; SM:  8; SD: 30; EY: 1068; EM:  9; ED: 28; DayOffset: lotDecOne),
    (SY: 1069; SM:  3; SD: 25; EY: 1069; EM:  4; ED: 23; DayOffset: lotDecOne),
    (SY: 1069; SM:  6; SD: 21; EY: 1069; EM:  7; ED: 20; DayOffset: lotDecOne),
    (SY: 1069; SM:  8; SD: 19; EY: 1069; EM:  9; ED: 17; DayOffset: lotDecOne),
    (SY: 1069; SM: 12; SD: 16; EY: 1070; EM:  1; ED: 14; DayOffset: lotDecOne),
    (SY: 1070; SM:  4; SD: 13; EY: 1070; EM:  5; ED: 12; DayOffset: lotDecOne),
    (SY: 1070; SM:  7; SD: 10; EY: 1070; EM:  8; ED:  8; DayOffset: lotDecOne),
    (SY: 1070; SM: 12; SD:  5; EY: 1071; EM:  1; ED:  3; DayOffset: lotDecOne),
    (SY: 1071; SM:  5; SD:  2; EY: 1071; EM:  5; ED: 31; DayOffset: lotDecOne),
    (SY: 1071; SM:  7; SD: 29; EY: 1071; EM:  8; ED: 27; DayOffset: lotDecOne),
    (SY: 1071; SM: 11; SD: 24; EY: 1071; EM: 12; ED: 23; DayOffset: lotDecOne),
    (SY: 1072; SM:  3; SD: 22; EY: 1072; EM:  4; ED: 20; DayOffset: lotDecOne),
    (SY: 1072; SM:  5; SD: 20; EY: 1072; EM:  6; ED: 18; DayOffset: lotDecOne),
    (SY: 1072; SM:  8; SD: 16; EY: 1072; EM:  9; ED: 14; DayOffset: lotDecOne),
    (SY: 1072; SM: 12; SD: 12; EY: 1073; EM:  1; ED: 10; DayOffset: lotDecOne),
    (SY: 1073; SM:  3; SD: 11; EY: 1073; EM:  4; ED:  9; DayOffset: lotDecOne),
    (SY: 1073; SM:  9; SD:  4; EY: 1073; EM: 10; ED:  3; DayOffset: lotDecOne),
    (SY: 1074; SM:  9; SD: 23; EY: 1074; EM: 10; ED: 22; DayOffset: lotDecOne),
    (SY: 1074; SM: 11; SD: 21; EY: 1074; EM: 12; ED: 20; DayOffset: lotDecOne),
    (SY: 1075; SM:  1; SD: 19; EY: 1075; EM:  2; ED: 17; DayOffset: lotDecOne),
    (SY: 1075; SM:  3; SD: 19; EY: 1075; EM:  4; ED: 17; DayOffset: lotDecOne),
    (SY: 1075; SM:  8; SD: 14; EY: 1075; EM:  9; ED: 12; DayOffset: lotDecOne),
    (SY: 1075; SM: 10; SD: 12; EY: 1075; EM: 11; ED: 10; DayOffset: lotDecOne),
    (SY: 1075; SM: 12; SD: 10; EY: 1076; EM:  1; ED:  8; DayOffset: lotDecOne),
    (SY: 1076; SM:  2; SD:  7; EY: 1076; EM:  3; ED:  7; DayOffset: lotDecOne),
    (SY: 1076; SM:  6; SD:  4; EY: 1076; EM:  7; ED:  3; DayOffset: lotDecOne),
    (SY: 1076; SM:  8; SD:  2; EY: 1076; EM:  8; ED: 31; DayOffset: lotDecOne),
    (SY: 1076; SM: 10; SD: 30; EY: 1076; EM: 11; ED: 28; DayOffset: lotDecOne),
    (SY: 1077; SM:  2; SD: 25; EY: 1077; EM:  3; ED: 26; DayOffset: lotDecOne),
    (SY: 1077; SM: 11; SD: 18; EY: 1077; EM: 12; ED: 17; DayOffset: lotDecOne),
    (SY: 1078; SM:  3; SD: 16; EY: 1078; EM:  4; ED: 14; DayOffset: lotDecOne),
    (SY: 1078; SM:  6; SD: 12; EY: 1078; EM:  7; ED: 11; DayOffset: lotDecOne),
    (SY: 1078; SM:  8; SD: 10; EY: 1078; EM:  9; ED:  8; DayOffset: lotDecOne),
    (SY: 1078; SM: 11; SD:  7; EY: 1078; EM: 12; ED:  6; DayOffset: lotDecOne),
    (SY: 1079; SM:  4; SD:  4; EY: 1079; EM:  5; ED:  3; DayOffset: lotDecOne),
    (SY: 1079; SM:  7; SD:  1; EY: 1079; EM:  7; ED: 30; DayOffset: lotDecOne),
    (SY: 1079; SM: 10; SD: 27; EY: 1079; EM: 11; ED: 25; DayOffset: lotDecOne),
    (SY: 1080; SM:  2; SD: 23; EY: 1080; EM:  3; ED: 23; DayOffset: lotDecOne),
    (SY: 1080; SM:  4; SD: 22; EY: 1080; EM:  5; ED: 21; DayOffset: lotDecOne),
    (SY: 1080; SM:  7; SD: 19; EY: 1080; EM:  8; ED: 17; DayOffset: lotDecOne),
    (SY: 1081; SM:  5; SD: 11; EY: 1081; EM:  6; ED:  9; DayOffset: lotDecOne),
    (SY: 1081; SM:  8; SD:  7; EY: 1081; EM:  9; ED:  5; DayOffset: lotDecOne),
    (SY: 1082; SM:  3; SD:  2; EY: 1082; EM:  3; ED: 31; DayOffset: lotDecOne),
    (SY: 1082; SM:  8; SD: 26; EY: 1082; EM:  9; ED: 24; DayOffset: lotDecOne),
    (SY: 1083; SM:  2; SD: 19; EY: 1083; EM:  3; ED: 20; DayOffset: lotDecOne),
    (SY: 1083; SM:  5; SD: 19; EY: 1083; EM:  6; ED: 17; DayOffset: lotDecOne),
    (SY: 1083; SM:  7; SD: 17; EY: 1083; EM:  8; ED: 15; DayOffset: lotDecOne),
    (SY: 1083; SM:  9; SD: 14; EY: 1083; EM: 10; ED: 13; DayOffset: lotDecOne),
    (SY: 1083; SM: 11; SD: 12; EY: 1083; EM: 12; ED: 11; DayOffset: lotDecOne),
    (SY: 1084; SM:  5; SD:  7; EY: 1084; EM:  6; ED:  5; DayOffset: lotDecOne),
    (SY: 1084; SM: 10; SD:  2; EY: 1084; EM: 10; ED: 31; DayOffset: lotDecOne),
    (SY: 1084; SM: 11; SD: 30; EY: 1084; EM: 12; ED: 29; DayOffset: lotDecOne),
    (SY: 1085; SM:  1; SD: 28; EY: 1085; EM:  2; ED: 26; DayOffset: lotDecOne),
    (SY: 1085; SM:  7; SD: 24; EY: 1085; EM:  8; ED: 22; DayOffset: lotDecOne),
    (SY: 1085; SM: 10; SD: 21; EY: 1085; EM: 11; ED: 19; DayOffset: lotDecOne),
    (SY: 1085; SM: 12; SD: 19; EY: 1086; EM:  1; ED: 17; DayOffset: lotDecOne),
    (SY: 1086; SM:  2; SD: 16; EY: 1086; EM:  3; ED: 17; DayOffset: lotDecOne),
    (SY: 1086; SM:  5; SD: 15; EY: 1086; EM:  6; ED: 13; DayOffset: lotDecOne),
    (SY: 1086; SM:  7; SD: 13; EY: 1086; EM:  8; ED: 11; DayOffset: lotDecOne),
    (SY: 1086; SM: 10; SD: 10; EY: 1086; EM: 11; ED:  8; DayOffset: lotDecOne),
    (SY: 1087; SM:  1; SD:  7; EY: 1087; EM:  2; ED:  5; DayOffset: lotDecOne),
    (SY: 1087; SM:  3; SD:  7; EY: 1087; EM:  4; ED:  5; DayOffset: lotDecOne),
    (SY: 1087; SM:  6; SD:  3; EY: 1087; EM:  7; ED:  2; DayOffset: lotDecOne),
    (SY: 1087; SM:  9; SD: 29; EY: 1087; EM: 10; ED: 28; DayOffset: lotDecOne),
    (SY: 1088; SM:  1; SD: 26; EY: 1088; EM:  2; ED: 24; DayOffset: lotDecOne),
    (SY: 1088; SM:  3; SD: 25; EY: 1088; EM:  4; ED: 23; DayOffset: lotDecOne),
    (SY: 1088; SM:  6; SD: 21; EY: 1088; EM:  7; ED: 20; DayOffset: lotDecOne),
    (SY: 1089; SM:  4; SD: 13; EY: 1089; EM:  5; ED: 12; DayOffset: lotDecOne),
    (SY: 1089; SM:  7; SD: 10; EY: 1089; EM:  8; ED:  8; DayOffset: lotDecOne),
    (SY: 1090; SM:  2; SD:  2; EY: 1090; EM:  3; ED:  3; DayOffset: lotDecOne),
    (SY: 1090; SM:  5; SD:  2; EY: 1090; EM:  5; ED: 31; DayOffset: lotDecOne),
    (SY: 1090; SM:  7; SD: 29; EY: 1090; EM:  8; ED: 27; DayOffset: lotDecOne),
    (SY: 1091; SM:  1; SD: 22; EY: 1091; EM:  2; ED: 20; DayOffset: lotDecOne),
    (SY: 1091; SM:  4; SD: 21; EY: 1091; EM:  5; ED: 20; DayOffset: lotDecOne),
    (SY: 1091; SM:  8; SD: 17; EY: 1091; EM:  9; ED: 15; DayOffset: lotDecOne),
    (SY: 1092; SM:  4; SD:  9; EY: 1092; EM:  5; ED:  8; DayOffset: lotDecOne),
    (SY: 1092; SM:  7; SD:  7; EY: 1092; EM:  8; ED:  5; DayOffset: lotDecOne),
    (SY: 1092; SM:  9; SD:  4; EY: 1092; EM: 10; ED:  3; DayOffset: lotDecOne),
    (SY: 1093; SM:  6; SD: 26; EY: 1093; EM:  7; ED: 25; DayOffset: lotDecOne),
    (SY: 1093; SM:  9; SD: 23; EY: 1093; EM: 10; ED: 22; DayOffset: lotDecOne),
    (SY: 1093; SM: 11; SD: 21; EY: 1093; EM: 12; ED: 20; DayOffset: lotDecOne),
    (SY: 1094; SM:  9; SD: 12; EY: 1094; EM: 10; ED: 11; DayOffset: lotDecOne),
    (SY: 1094; SM: 12; SD: 10; EY: 1095; EM:  1; ED:  8; DayOffset: lotDecOne),
    (SY: 1095; SM:  2; SD:  7; EY: 1095; EM:  3; ED:  8; DayOffset: lotDecOne),
    (SY: 1095; SM:  5; SD:  6; EY: 1095; EM:  6; ED:  4; DayOffset: lotDecOne),
    (SY: 1095; SM:  7; SD:  4; EY: 1095; EM:  8; ED:  2; DayOffset: lotDecOne),
    (SY: 1095; SM:  9; SD:  1; EY: 1095; EM:  9; ED: 30; DayOffset: lotDecOne),
    (SY: 1095; SM: 12; SD: 29; EY: 1096; EM:  1; ED: 27; DayOffset: lotDecOne),
    (SY: 1096; SM:  2; SD: 26; EY: 1096; EM:  3; ED: 26; DayOffset: lotDecOne),
    (SY: 1096; SM:  5; SD: 24; EY: 1096; EM:  6; ED: 22; DayOffset: lotDecOne),
    (SY: 1097; SM:  3; SD: 16; EY: 1097; EM:  4; ED: 14; DayOffset: lotDecOne),
    (SY: 1097; SM:  6; SD: 12; EY: 1097; EM:  7; ED: 11; DayOffset: lotDecOne),
    (SY: 1097; SM:  9; SD:  8; EY: 1097; EM: 10; ED:  7; DayOffset: lotDecOne),
    (SY: 1097; SM: 12; SD:  6; EY: 1098; EM:  2; ED:  3; DayOffset: lotDecOne),
    (SY: 1098; SM:  4; SD:  4; EY: 1098; EM:  5; ED:  3; DayOffset: lotDecOne),
    (SY: 1098; SM:  7; SD:  1; EY: 1098; EM:  7; ED: 30; DayOffset: lotDecOne),
    (SY: 1098; SM:  9; SD: 27; EY: 1098; EM: 10; ED: 26; DayOffset: lotDecOne),
    (SY: 1098; SM: 12; SD: 25; EY: 1099; EM:  1; ED: 23; DayOffset: lotDecOne),
    (SY: 1099; SM:  7; SD: 20; EY: 1099; EM:  8; ED: 18; DayOffset: lotDecOne),
    (SY: 1099; SM: 10; SD: 16; EY: 1099; EM: 11; ED: 14; DayOffset: lotDecOne),

    (SY: 1100; SM:  4; SD: 11; EY: 1100; EM:  5; ED: 10; DayOffset: lotDecOne),
    (SY: 1100; SM:  6; SD:  9; EY: 1100; EM:  7; ED:  8; DayOffset: lotDecOne),
    (SY: 1100; SM:  8; SD:  7; EY: 1100; EM:  9; ED:  5; DayOffset: lotDecOne),
    (SY: 1101; SM:  3; SD: 31; EY: 1101; EM:  4; ED: 29; DayOffset: lotDecOne),
    (SY: 1101; SM:  8; SD: 26; EY: 1101; EM:  9; ED: 24; DayOffset: lotDecOne),
    (SY: 1102; SM:  6; SD: 17; EY: 1102; EM:  7; ED: 16; DayOffset: lotDecOne),
    (SY: 1103; SM:  4; SD:  8; EY: 1103; EM:  5; ED:  7; DayOffset: lotDecOne),
    (SY: 1103; SM:  6; SD:  6; EY: 1103; EM:  7; ED:  5; DayOffset: lotDecOne),
    (SY: 1103; SM: 12; SD:  1; EY: 1103; EM: 12; ED: 30; DayOffset: lotDecOne),
    (SY: 1104; SM:  1; SD: 29; EY: 1104; EM:  2; ED: 27; DayOffset: lotDecOne),
    (SY: 1104; SM:  4; SD: 26; EY: 1104; EM:  5; ED: 25; DayOffset: lotDecOne),
    (SY: 1104; SM:  8; SD: 22; EY: 1104; EM:  9; ED: 20; DayOffset: lotDecOne),
    (SY: 1104; SM: 11; SD: 19; EY: 1104; EM: 12; ED: 18; DayOffset: lotDecOne),
    (SY: 1105; SM:  2; SD: 16; EY: 1105; EM:  3; ED: 17; DayOffset: lotDecOne),
    (SY: 1105; SM:  5; SD: 15; EY: 1105; EM:  6; ED: 13; DayOffset: lotDecOne),
    (SY: 1105; SM:  8; SD: 11; EY: 1105; EM:  9; ED:  9; DayOffset: lotDecOne),
    (SY: 1105; SM: 11; SD:  8; EY: 1106; EM:  1; ED:  6; DayOffset: lotDecOne),
    (SY: 1106; SM:  3; SD:  7; EY: 1106; EM:  4; ED:  5; DayOffset: lotDecOne),
    (SY: 1106; SM:  6; SD:  3; EY: 1106; EM:  7; ED:  2; DayOffset: lotDecOne),
    (SY: 1106; SM:  8; SD: 30; EY: 1106; EM:  9; ED: 28; DayOffset: lotDecOne),
    (SY: 1107; SM:  6; SD: 22; EY: 1107; EM:  7; ED: 21; DayOffset: lotDecOne),
    (SY: 1107; SM:  9; SD: 18; EY: 1107; EM: 10; ED: 17; DayOffset: lotDecOne),
    (SY: 1107; SM: 11; SD: 16; EY: 1107; EM: 12; ED: 15; DayOffset: lotDecOne),
    (SY: 1108; SM:  3; SD: 14; EY: 1108; EM:  4; ED: 12; DayOffset: lotDecOne),
    (SY: 1108; SM:  7; SD: 10; EY: 1108; EM:  8; ED:  8; DayOffset: lotDecOne),
    (SY: 1108; SM: 10; SD:  6; EY: 1108; EM: 11; ED:  4; DayOffset: lotDecOne),
    (SY: 1108; SM: 12; SD:  4; EY: 1109; EM:  1; ED:  2; DayOffset: lotDecOne),
    (SY: 1109; SM:  3; SD:  3; EY: 1109; EM:  4; ED:  1; DayOffset: lotDecOne),
    (SY: 1109; SM:  5; SD: 31; EY: 1109; EM:  6; ED: 29; DayOffset: lotDecOne),
    (SY: 1109; SM:  7; SD: 29; EY: 1109; EM:  8; ED: 27; DayOffset: lotDecOne),
    (SY: 1109; SM: 12; SD: 23; EY: 1110; EM:  1; ED: 21; DayOffset: lotDecOne),
    (SY: 1110; SM:  5; SD: 20; EY: 1110; EM:  6; ED: 18; DayOffset: lotDecOne),
    (SY: 1111; SM:  3; SD: 11; EY: 1111; EM:  4; ED:  9; DayOffset: lotDecOne),
    (SY: 1111; SM:  5; SD:  9; EY: 1111; EM:  6; ED:  7; DayOffset: lotDecOne),
    (SY: 1111; SM:  8; SD:  6; EY: 1111; EM:  9; ED:  4; DayOffset: lotDecOne),
    (SY: 1112; SM:  3; SD: 29; EY: 1112; EM:  4; ED: 27; DayOffset: lotDecOne),
    (SY: 1112; SM:  5; SD: 27; EY: 1112; EM:  6; ED: 25; DayOffset: lotDecOne),
    (SY: 1112; SM:  7; SD: 25; EY: 1112; EM:  8; ED: 23; DayOffset: lotDecOne),
    (SY: 1112; SM: 10; SD: 22; EY: 1112; EM: 11; ED: 20; DayOffset: lotDecOne),
    (SY: 1113; SM:  1; SD: 19; EY: 1113; EM:  2; ED: 17; DayOffset: lotDecOne),
    (SY: 1113; SM:  4; SD: 17; EY: 1113; EM:  5; ED: 16; DayOffset: lotDecOne),
    (SY: 1114; SM:  2; SD:  7; EY: 1114; EM:  3; ED:  8; DayOffset: lotDecOne),
    (SY: 1114; SM:  5; SD:  6; EY: 1114; EM:  6; ED:  4; DayOffset: lotDecOne),
    (SY: 1114; SM:  8; SD:  2; EY: 1114; EM:  8; ED: 31; DayOffset: lotDecOne),
    (SY: 1115; SM:  5; SD: 25; EY: 1115; EM:  6; ED: 23; DayOffset: lotDecOne),
    (SY: 1115; SM:  8; SD: 21; EY: 1115; EM:  9; ED: 19; DayOffset: lotDecOne),
    (SY: 1115; SM: 10; SD: 19; EY: 1115; EM: 11; ED: 17; DayOffset: lotDecOne),
    (SY: 1116; SM:  2; SD: 15; EY: 1116; EM:  3; ED: 15; DayOffset: lotDecOne),
    (SY: 1116; SM:  6; SD: 12; EY: 1116; EM:  7; ED: 11; DayOffset: lotDecOne),
    (SY: 1116; SM:  9; SD:  8; EY: 1116; EM: 10; ED:  7; DayOffset: lotDecOne),
    (SY: 1116; SM: 11; SD:  6; EY: 1116; EM: 12; ED:  5; DayOffset: lotDecOne),
    (SY: 1117; SM:  2; SD:  3; EY: 1117; EM:  3; ED:  4; DayOffset: lotDecOne),
    (SY: 1117; SM:  5; SD:  3; EY: 1117; EM:  6; ED:  1; DayOffset: lotDecOne),
    (SY: 1117; SM:  7; SD:  1; EY: 1117; EM:  7; ED: 30; DayOffset: lotDecOne),
    (SY: 1117; SM:  9; SD: 27; EY: 1117; EM: 10; ED: 26; DayOffset: lotDecOne),
    (SY: 1117; SM: 11; SD: 25; EY: 1117; EM: 12; ED: 24; DayOffset: lotDecOne),
    (SY: 1118; SM: 10; SD: 16; EY: 1118; EM: 11; ED: 14; DayOffset: lotDecOne),
    (SY: 1118; SM: 12; SD: 14; EY: 1119; EM:  1; ED: 12; DayOffset: lotDecOne),
    (SY: 1119; SM:  2; SD: 11; EY: 1119; EM:  3; ED: 12; DayOffset: lotDecOne),
    (SY: 1120; SM:  1; SD:  2; EY: 1120; EM:  1; ED: 31; DayOffset: lotDecOne),
    (SY: 1120; SM:  3; SD:  1; EY: 1120; EM:  3; ED: 30; DayOffset: lotDecOne),
    (SY: 1120; SM:  4; SD: 29; EY: 1120; EM:  5; ED: 28; DayOffset: lotDecOne),
    (SY: 1120; SM:  9; SD: 24; EY: 1120; EM: 10; ED: 23; DayOffset: lotDecOne),
    (SY: 1121; SM:  3; SD: 20; EY: 1121; EM:  4; ED: 18; DayOffset: lotDecOne),
    (SY: 1122; SM:  4; SD:  8; EY: 1122; EM:  5; ED:  7; DayOffset: lotDecOne),
    (SY: 1122; SM:  7; SD:  5; EY: 1122; EM:  8; ED:  3; DayOffset: lotDecOne),
    (SY: 1122; SM: 10; SD:  2; EY: 1122; EM: 10; ED: 31; DayOffset: lotDecOne),
    (SY: 1123; SM:  4; SD: 27; EY: 1123; EM:  5; ED: 26; DayOffset: lotDecOne),
    (SY: 1123; SM:  7; SD: 24; EY: 1123; EM:  8; ED: 22; DayOffset: lotDecOne),
    (SY: 1123; SM:  9; SD: 21; EY: 1123; EM: 10; ED: 20; DayOffset: lotDecOne),
    (SY: 1124; SM:  1; SD: 18; EY: 1124; EM:  2; ED: 16; DayOffset: lotDecOne),
    (SY: 1124; SM:  5; SD: 15; EY: 1124; EM:  6; ED: 13; DayOffset: lotDecOne),
    (SY: 1124; SM:  8; SD: 11; EY: 1124; EM:  9; ED:  9; DayOffset: lotDecOne),
    (SY: 1125; SM:  1; SD:  6; EY: 1125; EM:  2; ED:  4; DayOffset: lotDecOne),
    (SY: 1125; SM:  6; SD:  3; EY: 1125; EM:  7; ED:  2; DayOffset: lotDecOne),
    (SY: 1125; SM:  8; SD: 30; EY: 1125; EM:  9; ED: 28; DayOffset: lotDecOne),
    (SY: 1125; SM: 12; SD: 26; EY: 1126; EM:  1; ED: 24; DayOffset: lotDecOne),
    (SY: 1126; SM:  4; SD: 24; EY: 1126; EM:  5; ED: 23; DayOffset: lotDecOne),
    (SY: 1126; SM:  6; SD: 22; EY: 1126; EM:  7; ED: 21; DayOffset: lotDecOne),
    (SY: 1126; SM:  9; SD: 18; EY: 1126; EM: 10; ED: 17; DayOffset: lotDecOne),
    (SY: 1127; SM:  1; SD: 14; EY: 1127; EM:  2; ED: 12; DayOffset: lotDecOne),
    (SY: 1127; SM:  4; SD: 13; EY: 1127; EM:  5; ED: 12; DayOffset: lotDecOne),
    (SY: 1127; SM: 10; SD:  7; EY: 1127; EM: 11; ED:  5; DayOffset: lotDecOne),
    (SY: 1127; SM: 12; SD:  5; EY: 1128; EM:  1; ED:  3; DayOffset: lotDecOne),
    (SY: 1128; SM:  2; SD:  2; EY: 1128; EM:  3; ED:  2; DayOffset: lotDecOne),
    (SY: 1128; SM:  4; SD:  1; EY: 1128; EM:  4; ED: 30; DayOffset: lotDecOne),
    (SY: 1128; SM:  6; SD: 29; EY: 1128; EM:  7; ED: 28; DayOffset: lotDecOne),
    (SY: 1128; SM:  8; SD: 27; EY: 1128; EM:  9; ED: 25; DayOffset: lotDecOne),
    (SY: 1128; SM: 12; SD: 23; EY: 1129; EM:  1; ED: 21; DayOffset: lotDecOne),
    (SY: 1129; SM:  2; SD: 20; EY: 1129; EM:  3; ED: 21; DayOffset: lotDecOne),
    (SY: 1129; SM:  6; SD: 18; EY: 1129; EM:  7; ED: 17; DayOffset: lotDecOne),
    (SY: 1129; SM:  9; SD: 15; EY: 1129; EM: 10; ED: 14; DayOffset: lotDecOne),
    (SY: 1130; SM:  3; SD: 11; EY: 1130; EM:  4; ED:  9; DayOffset: lotDecOne),
    (SY: 1130; SM:  9; SD:  4; EY: 1130; EM: 10; ED:  3; DayOffset: lotDecOne),
    (SY: 1131; SM:  3; SD: 30; EY: 1131; EM:  4; ED: 28; DayOffset: lotDecOne),
    (SY: 1131; SM:  6; SD: 26; EY: 1131; EM:  7; ED: 25; DayOffset: lotDecOne),
    (SY: 1131; SM:  8; SD: 24; EY: 1131; EM:  9; ED: 22; DayOffset: lotDecOne),
    (SY: 1131; SM: 12; SD: 21; EY: 1132; EM:  1; ED: 19; DayOffset: lotDecOne),
    (SY: 1132; SM:  4; SD: 17; EY: 1132; EM:  5; ED: 16; DayOffset: lotDecOne),
    (SY: 1132; SM:  7; SD: 14; EY: 1132; EM:  8; ED: 12; DayOffset: lotDecOne),
    (SY: 1132; SM: 12; SD:  9; EY: 1133; EM:  1; ED:  7; DayOffset: lotDecOne),
    (SY: 1133; SM:  5; SD:  6; EY: 1133; EM:  6; ED:  4; DayOffset: lotDecOne),
    (SY: 1133; SM:  8; SD:  2; EY: 1133; EM:  8; ED: 31; DayOffset: lotDecOne),
    (SY: 1133; SM: 11; SD: 28; EY: 1133; EM: 12; ED: 27; DayOffset: lotDecOne),
    (SY: 1134; SM:  3; SD: 27; EY: 1134; EM:  4; ED: 25; DayOffset: lotDecOne),
    (SY: 1134; SM:  5; SD: 25; EY: 1134; EM:  6; ED: 23; DayOffset: lotDecOne),
    (SY: 1134; SM:  8; SD: 21; EY: 1134; EM:  9; ED: 19; DayOffset: lotDecOne),
    (SY: 1135; SM:  3; SD: 16; EY: 1135; EM:  4; ED: 14; DayOffset: lotDecOne),
    (SY: 1135; SM:  6; SD: 13; EY: 1135; EM:  7; ED: 12; DayOffset: lotDecOne),
    (SY: 1135; SM:  9; SD:  9; EY: 1135; EM: 10; ED:  8; DayOffset: lotDecOne),
    (SY: 1136; SM:  3; SD:  4; EY: 1136; EM:  4; ED:  2; DayOffset: lotDecOne),
    (SY: 1136; SM:  6; SD:  1; EY: 1136; EM:  6; ED: 30; DayOffset: lotDecOne),
    (SY: 1136; SM:  9; SD: 27; EY: 1136; EM: 10; ED: 26; DayOffset: lotDecOne),
    (SY: 1136; SM: 11; SD: 25; EY: 1136; EM: 12; ED: 24; DayOffset: lotDecOne),
    (SY: 1137; SM:  3; SD: 23; EY: 1137; EM:  4; ED: 21; DayOffset: lotDecOne),
    (SY: 1137; SM:  8; SD: 18; EY: 1137; EM:  9; ED: 16; DayOffset: lotDecOne),
    (SY: 1137; SM: 10; SD: 16; EY: 1137; EM: 11; ED: 14; DayOffset: lotDecOne),
    (SY: 1137; SM: 12; SD: 14; EY: 1138; EM:  1; ED: 12; DayOffset: lotDecOne),
    (SY: 1138; SM:  2; SD: 11; EY: 1138; EM:  3; ED: 12; DayOffset: lotDecOne),
    (SY: 1138; SM:  8; SD:  7; EY: 1138; EM:  9; ED:  5; DayOffset: lotDecOne),
    (SY: 1138; SM: 11; SD:  4; EY: 1138; EM: 12; ED:  3; DayOffset: lotDecOne),
    (SY: 1139; SM:  3; SD:  2; EY: 1139; EM:  3; ED: 31; DayOffset: lotDecOne),
    (SY: 1139; SM:  5; SD: 29; EY: 1139; EM:  6; ED: 27; DayOffset: lotDecOne),
    (SY: 1139; SM:  7; SD: 27; EY: 1139; EM:  8; ED: 25; DayOffset: lotDecOne),
    (SY: 1139; SM: 11; SD: 23; EY: 1139; EM: 12; ED: 22; DayOffset: lotDecOne),
    (SY: 1140; SM:  3; SD: 20; EY: 1140; EM:  4; ED: 18; DayOffset: lotDecOne),
    (SY: 1140; SM:  6; SD: 16; EY: 1140; EM:  7; ED: 15; DayOffset: lotDecOne),
    (SY: 1140; SM:  8; SD: 14; EY: 1140; EM:  9; ED: 12; DayOffset: lotDecOne),
    (SY: 1140; SM: 11; SD: 11; EY: 1140; EM: 12; ED: 10; DayOffset: lotDecOne),
    (SY: 1141; SM:  4; SD:  8; EY: 1141; EM:  5; ED:  7; DayOffset: lotDecOne),
    (SY: 1141; SM:  7; SD:  5; EY: 1141; EM:  8; ED:  3; DayOffset: lotDecOne),
    (SY: 1141; SM: 10; SD: 31; EY: 1141; EM: 11; ED: 29; DayOffset: lotDecOne),
    (SY: 1142; SM:  2; SD: 27; EY: 1142; EM:  3; ED: 28; DayOffset: lotDecOne),
    (SY: 1142; SM:  4; SD: 27; EY: 1142; EM:  5; ED: 26; DayOffset: lotDecOne),
    (SY: 1142; SM:  7; SD: 24; EY: 1142; EM:  8; ED: 22; DayOffset: lotDecOne),
    (SY: 1143; SM:  5; SD: 16; EY: 1143; EM:  6; ED: 14; DayOffset: lotDecOne),
    (SY: 1143; SM:  8; SD: 12; EY: 1143; EM:  9; ED: 10; DayOffset: lotDecOne),
    (SY: 1144; SM:  8; SD: 30; EY: 1144; EM:  9; ED: 28; DayOffset: lotDecOne),
    (SY: 1145; SM:  2; SD: 23; EY: 1145; EM:  3; ED: 24; DayOffset: lotDecOne),
    (SY: 1145; SM:  5; SD: 23; EY: 1145; EM:  6; ED: 21; DayOffset: lotDecOne),
    (SY: 1145; SM:  7; SD: 21; EY: 1145; EM:  8; ED: 19; DayOffset: lotDecOne),
    (SY: 1145; SM:  9; SD: 18; EY: 1145; EM: 10; ED: 17; DayOffset: lotDecOne),
    (SY: 1146; SM:  5; SD: 12; EY: 1146; EM:  6; ED: 10; DayOffset: lotDecOne),
    (SY: 1146; SM: 10; SD:  7; EY: 1146; EM: 11; ED:  5; DayOffset: lotDecOne),
    (SY: 1146; SM: 12; SD:  5; EY: 1147; EM:  1; ED:  3; DayOffset: lotDecOne),
    (SY: 1147; SM: 12; SD: 24; EY: 1148; EM:  1; ED: 22; DayOffset: lotDecOne),
    (SY: 1148; SM:  2; SD: 21; EY: 1148; EM:  3; ED: 21; DayOffset: lotDecOne),
    (SY: 1148; SM:  5; SD: 19; EY: 1148; EM:  6; ED: 17; DayOffset: lotDecOne),
    (SY: 1148; SM:  7; SD: 17; EY: 1148; EM:  8; ED: 15; DayOffset: lotDecOne),
    (SY: 1148; SM: 10; SD: 14; EY: 1148; EM: 11; ED: 12; DayOffset: lotDecOne),
    (SY: 1149; SM:  1; SD: 11; EY: 1149; EM:  2; ED:  9; DayOffset: lotDecOne),
    (SY: 1149; SM:  3; SD: 11; EY: 1149; EM:  4; ED:  9; DayOffset: lotDecOne),
    (SY: 1149; SM:  6; SD:  7; EY: 1149; EM:  7; ED:  6; DayOffset: lotDecOne),
    (SY: 1149; SM: 10; SD:  3; EY: 1149; EM: 11; ED:  1; DayOffset: lotDecOne),
    (SY: 1150; SM:  1; SD: 30; EY: 1150; EM:  2; ED: 28; DayOffset: lotDecOne),
    (SY: 1150; SM:  3; SD: 30; EY: 1150; EM:  4; ED: 28; DayOffset: lotDecOne),
    (SY: 1150; SM:  6; SD: 26; EY: 1150; EM:  7; ED: 25; DayOffset: lotDecOne),
    (SY: 1151; SM:  4; SD: 18; EY: 1151; EM:  5; ED: 17; DayOffset: lotDecOne),
    (SY: 1151; SM:  7; SD: 15; EY: 1151; EM:  8; ED: 13; DayOffset: lotDecOne),
    (SY: 1151; SM: 10; SD: 11; EY: 1151; EM: 11; ED:  9; DayOffset: lotDecOne),
    (SY: 1152; SM:  1; SD:  8; EY: 1152; EM:  3; ED:  7; DayOffset: lotDecOne),
    (SY: 1152; SM:  8; SD:  2; EY: 1152; EM:  8; ED: 31; DayOffset: lotDecOne),
    (SY: 1152; SM: 10; SD: 29; EY: 1152; EM: 11; ED: 27; DayOffset: lotDecOne),
    (SY: 1153; SM:  1; SD: 26; EY: 1153; EM:  2; ED: 24; DayOffset: lotDecOne),
    (SY: 1153; SM:  4; SD: 25; EY: 1153; EM:  5; ED: 24; DayOffset: lotDecOne),
    (SY: 1153; SM:  8; SD: 21; EY: 1153; EM:  9; ED: 19; DayOffset: lotDecOne),
    (SY: 1154; SM:  4; SD: 14; EY: 1154; EM:  5; ED: 13; DayOffset: lotDecOne),
    (SY: 1154; SM:  7; SD: 12; EY: 1154; EM:  8; ED: 10; DayOffset: lotDecOne),
    (SY: 1154; SM:  9; SD:  9; EY: 1154; EM: 10; ED:  8; DayOffset: lotDecOne),
    (SY: 1155; SM:  7; SD:  1; EY: 1155; EM:  7; ED: 30; DayOffset: lotDecOne),
    (SY: 1155; SM: 11; SD: 26; EY: 1155; EM: 12; ED: 25; DayOffset: lotDecOne),
    (SY: 1156; SM:  4; SD: 21; EY: 1156; EM:  5; ED: 20; DayOffset: lotDecOne),
    (SY: 1156; SM:  6; SD: 19; EY: 1156; EM:  7; ED: 18; DayOffset: lotDecOne),
    (SY: 1156; SM:  9; SD: 16; EY: 1156; EM: 10; ED: 15; DayOffset: lotDecOne),
    (SY: 1156; SM: 12; SD: 14; EY: 1157; EM:  1; ED: 12; DayOffset: lotDecOne),
    (SY: 1157; SM:  2; SD: 11; EY: 1157; EM:  3; ED: 12; DayOffset: lotDecOne),
    (SY: 1157; SM:  5; SD: 10; EY: 1157; EM:  6; ED:  8; DayOffset: lotDecOne),
    (SY: 1157; SM:  9; SD:  5; EY: 1157; EM: 10; ED:  4; DayOffset: lotDecOne),
    (SY: 1158; SM:  1; SD:  2; EY: 1158; EM:  1; ED: 31; DayOffset: lotDecOne),
    (SY: 1158; SM:  3; SD:  2; EY: 1158; EM:  3; ED: 31; DayOffset: lotDecOne),
    (SY: 1158; SM:  5; SD: 29; EY: 1158; EM:  6; ED: 27; DayOffset: lotDecOne),
    (SY: 1159; SM:  3; SD: 21; EY: 1159; EM:  4; ED: 19; DayOffset: lotDecOne),
    (SY: 1159; SM:  6; SD: 17; EY: 1159; EM:  7; ED: 16; DayOffset: lotDecOne),
    (SY: 1159; SM:  9; SD: 13; EY: 1159; EM: 10; ED: 12; DayOffset: lotDecOne),
    (SY: 1159; SM: 12; SD: 11; EY: 1160; EM:  2; ED:  8; DayOffset: lotDecOne),
    (SY: 1160; SM:  4; SD:  8; EY: 1160; EM:  5; ED:  7; DayOffset: lotDecOne),
    (SY: 1160; SM:  7; SD:  5; EY: 1160; EM:  8; ED:  3; DayOffset: lotDecOne),
    (SY: 1160; SM: 10; SD:  1; EY: 1160; EM: 10; ED: 30; DayOffset: lotDecOne),
    (SY: 1160; SM: 12; SD: 29; EY: 1161; EM:  1; ED: 27; DayOffset: lotDecOne),
    (SY: 1161; SM:  7; SD: 24; EY: 1161; EM:  8; ED: 22; DayOffset: lotDecOne),
    (SY: 1161; SM: 10; SD: 20; EY: 1161; EM: 11; ED: 18; DayOffset: lotDecOne),
    (SY: 1161; SM: 12; SD: 18; EY: 1162; EM:  1; ED: 16; DayOffset: lotDecOne),
    (SY: 1162; SM:  6; SD: 14; EY: 1162; EM:  7; ED: 13; DayOffset: lotDecOne),
    (SY: 1162; SM:  8; SD: 12; EY: 1162; EM:  9; ED: 10; DayOffset: lotDecOne),
    (SY: 1163; SM:  1; SD:  6; EY: 1163; EM:  2; ED:  4; DayOffset: lotDecOne),
    (SY: 1163; SM:  4; SD:  5; EY: 1163; EM:  5; ED:  4; DayOffset: lotDecOne),
    (SY: 1164; SM:  3; SD: 24; EY: 1164; EM:  4; ED: 22; DayOffset: lotDecOne),
    (SY: 1165; SM:  4; SD: 12; EY: 1165; EM:  5; ED: 11; DayOffset: lotDecOne),
    (SY: 1165; SM:  6; SD: 10; EY: 1165; EM:  7; ED:  9; DayOffset: lotDecOne),
    (SY: 1166; SM:  2; SD:  2; EY: 1166; EM:  3; ED:  3; DayOffset: lotDecOne),
    (SY: 1166; SM:  5; SD:  1; EY: 1166; EM:  5; ED: 30; DayOffset: lotDecOne),
    (SY: 1166; SM: 11; SD: 24; EY: 1166; EM: 12; ED: 23; DayOffset: lotDecOne),
    (SY: 1167; SM:  2; SD: 21; EY: 1167; EM:  3; ED: 22; DayOffset: lotDecOne),
    (SY: 1167; SM:  5; SD: 20; EY: 1167; EM:  6; ED: 18; DayOffset: lotDecOne),
    (SY: 1167; SM:  8; SD: 16; EY: 1167; EM:  9; ED: 14; DayOffset: lotDecOne),
    (SY: 1167; SM: 11; SD: 13; EY: 1168; EM:  1; ED: 11; DayOffset: lotDecOne),
    (SY: 1168; SM:  3; SD: 11; EY: 1168; EM:  4; ED:  9; DayOffset: lotDecOne),
    (SY: 1168; SM:  6; SD:  7; EY: 1168; EM:  7; ED:  6; DayOffset: lotDecOne),
    (SY: 1168; SM:  9; SD:  3; EY: 1168; EM: 10; ED:  2; DayOffset: lotDecOne),
    (SY: 1168; SM: 11; SD:  1; EY: 1168; EM: 11; ED: 30; DayOffset: lotIncOne),
    (SY: 1169; SM:  6; SD: 26; EY: 1169; EM:  7; ED: 25; DayOffset: lotDecOne),
    (SY: 1169; SM:  9; SD: 22; EY: 1169; EM: 10; ED: 21; DayOffset: lotDecOne),
    (SY: 1169; SM: 11; SD: 20; EY: 1169; EM: 12; ED: 19; DayOffset: lotDecOne),
    (SY: 1170; SM:  3; SD: 19; EY: 1170; EM:  4; ED: 17; DayOffset: lotDecOne),
    (SY: 1170; SM:  7; SD: 15; EY: 1170; EM:  8; ED: 13; DayOffset: lotDecOne),
    (SY: 1170; SM: 10; SD: 11; EY: 1170; EM: 11; ED:  9; DayOffset: lotDecOne),
    (SY: 1170; SM: 12; SD:  9; EY: 1171; EM:  1; ED:  7; DayOffset: lotDecOne),
    (SY: 1171; SM:  3; SD:  8; EY: 1171; EM:  4; ED:  6; DayOffset: lotDecOne),
    (SY: 1171; SM:  6; SD:  5; EY: 1171; EM:  7; ED:  4; DayOffset: lotDecOne),
    (SY: 1171; SM:  8; SD:  3; EY: 1171; EM:  9; ED:  1; DayOffset: lotDecOne),
    (SY: 1171; SM: 10; SD: 30; EY: 1171; EM: 11; ED: 28; DayOffset: lotDecOne),
    (SY: 1171; SM: 12; SD: 28; EY: 1172; EM:  1; ED: 26; DayOffset: lotDecOne),
    (SY: 1172; SM:  5; SD: 24; EY: 1172; EM:  6; ED: 22; DayOffset: lotDecOne),
    (SY: 1173; SM:  1; SD: 15; EY: 1173; EM:  2; ED: 13; DayOffset: lotDecOne),
    (SY: 1173; SM:  3; SD: 15; EY: 1173; EM:  4; ED: 13; DayOffset: lotDecOne),
    (SY: 1173; SM:  5; SD: 13; EY: 1173; EM:  6; ED: 11; DayOffset: lotDecOne),
    (SY: 1173; SM:  8; SD: 10; EY: 1173; EM:  9; ED:  8; DayOffset: lotDecOne),
    (SY: 1174; SM:  4; SD:  3; EY: 1174; EM:  5; ED:  2; DayOffset: lotDecOne),
    (SY: 1174; SM:  7; SD: 30; EY: 1174; EM:  8; ED: 28; DayOffset: lotDecOne),
    (SY: 1174; SM: 10; SD: 27; EY: 1174; EM: 11; ED: 25; DayOffset: lotDecOne),
    (SY: 1175; SM:  1; SD: 24; EY: 1175; EM:  2; ED: 22; DayOffset: lotDecOne),
    (SY: 1175; SM:  4; SD: 22; EY: 1175; EM:  5; ED: 21; DayOffset: lotDecOne),
    (SY: 1175; SM: 10; SD: 16; EY: 1175; EM: 11; ED: 14; DayOffset: lotDecOne),
    (SY: 1176; SM:  5; SD: 10; EY: 1176; EM:  6; ED:  8; DayOffset: lotDecOne),
    (SY: 1176; SM:  8; SD:  6; EY: 1176; EM:  9; ED:  4; DayOffset: lotDecOne),
    (SY: 1176; SM: 10; SD:  4; EY: 1176; EM: 11; ED:  2; DayOffset: lotDecOne),
    (SY: 1177; SM:  5; SD: 29; EY: 1177; EM:  6; ED: 27; DayOffset: lotDecOne),
    (SY: 1177; SM:  8; SD: 25; EY: 1177; EM:  9; ED: 23; DayOffset: lotDecOne),
    (SY: 1177; SM: 10; SD: 23; EY: 1177; EM: 11; ED: 21; DayOffset: lotDecOne),
    (SY: 1178; SM:  2; SD: 19; EY: 1178; EM:  3; ED: 20; DayOffset: lotDecOne),
    (SY: 1178; SM:  6; SD: 17; EY: 1178; EM:  7; ED: 16; DayOffset: lotDecOne),
    (SY: 1178; SM:  9; SD: 13; EY: 1178; EM: 10; ED: 12; DayOffset: lotDecOne),
    (SY: 1178; SM: 11; SD: 11; EY: 1178; EM: 12; ED: 10; DayOffset: lotDecOne),
    (SY: 1179; SM:  2; SD:  8; EY: 1179; EM:  3; ED:  9; DayOffset: lotDecOne),
    (SY: 1179; SM:  5; SD:  8; EY: 1179; EM:  6; ED:  6; DayOffset: lotDecOne),
    (SY: 1179; SM:  7; SD:  6; EY: 1179; EM:  8; ED:  4; DayOffset: lotDecOne),
    (SY: 1179; SM: 10; SD:  2; EY: 1179; EM: 10; ED: 31; DayOffset: lotDecOne),
    (SY: 1179; SM: 11; SD: 30; EY: 1179; EM: 12; ED: 29; DayOffset: lotDecOne),
    (SY: 1180; SM:  1; SD: 28; EY: 1180; EM:  2; ED: 26; DayOffset: lotDecOne),
    (SY: 1180; SM: 10; SD: 20; EY: 1180; EM: 11; ED: 18; DayOffset: lotDecOne),
    (SY: 1180; SM: 12; SD: 18; EY: 1181; EM:  1; ED: 16; DayOffset: lotDecOne),
    (SY: 1181; SM:  2; SD: 15; EY: 1181; EM:  3; ED: 16; DayOffset: lotDecOne),
    (SY: 1181; SM:  7; SD: 13; EY: 1181; EM:  8; ED: 11; DayOffset: lotDecOne),
    (SY: 1182; SM:  1; SD:  6; EY: 1182; EM:  2; ED:  4; DayOffset: lotDecOne),
    (SY: 1182; SM:  3; SD:  6; EY: 1182; EM:  4; ED:  4; DayOffset: lotDecOne),
    (SY: 1182; SM:  5; SD:  4; EY: 1182; EM:  6; ED:  2; DayOffset: lotDecOne),
    (SY: 1182; SM:  7; SD:  2; EY: 1182; EM:  7; ED: 31; DayOffset: lotDecOne),
    (SY: 1182; SM:  9; SD: 29; EY: 1182; EM: 10; ED: 28; DayOffset: lotDecOne),
    (SY: 1183; SM:  3; SD: 25; EY: 1183; EM:  4; ED: 23; DayOffset: lotDecOne),
    (SY: 1184; SM:  4; SD: 12; EY: 1184; EM:  5; ED: 11; DayOffset: lotDecOne),
    (SY: 1184; SM:  7; SD:  9; EY: 1184; EM:  8; ED:  7; DayOffset: lotDecOne),
    (SY: 1185; SM:  5; SD:  1; EY: 1185; EM:  5; ED: 30; DayOffset: lotDecOne),
    (SY: 1185; SM:  7; SD: 28; EY: 1185; EM:  8; ED: 26; DayOffset: lotDecOne),
    (SY: 1185; SM:  9; SD: 25; EY: 1185; EM: 10; ED: 24; DayOffset: lotDecOne),
    (SY: 1186; SM:  1; SD: 22; EY: 1186; EM:  2; ED: 20; DayOffset: lotDecOne),
    (SY: 1186; SM:  5; SD: 20; EY: 1186; EM:  6; ED: 18; DayOffset: lotDecOne),
    (SY: 1186; SM:  8; SD: 16; EY: 1186; EM:  9; ED: 14; DayOffset: lotDecOne),
    (SY: 1187; SM:  1; SD: 11; EY: 1187; EM:  2; ED:  9; DayOffset: lotDecOne),
    (SY: 1187; SM:  6; SD:  8; EY: 1187; EM:  7; ED:  7; DayOffset: lotDecOne),
    (SY: 1187; SM:  9; SD:  4; EY: 1187; EM: 10; ED:  3; DayOffset: lotDecOne),
    (SY: 1187; SM: 12; SD: 31; EY: 1188; EM:  1; ED: 29; DayOffset: lotDecOne),
    (SY: 1188; SM:  4; SD: 28; EY: 1188; EM:  5; ED: 27; DayOffset: lotDecOne),
    (SY: 1188; SM:  6; SD: 26; EY: 1188; EM:  7; ED: 25; DayOffset: lotDecOne),
    (SY: 1188; SM:  9; SD: 22; EY: 1188; EM: 10; ED: 21; DayOffset: lotDecOne),
    (SY: 1189; SM:  1; SD: 18; EY: 1189; EM:  2; ED: 16; DayOffset: lotDecOne),
    (SY: 1189; SM:  4; SD: 17; EY: 1189; EM:  5; ED: 16; DayOffset: lotDecOne),
    (SY: 1189; SM: 10; SD: 11; EY: 1189; EM: 11; ED:  9; DayOffset: lotDecOne),
    (SY: 1189; SM: 12; SD:  9; EY: 1190; EM:  1; ED:  7; DayOffset: lotDecOne),
    (SY: 1190; SM:  2; SD:  6; EY: 1190; EM:  3; ED:  7; DayOffset: lotDecOne),
    (SY: 1190; SM:  4; SD:  6; EY: 1190; EM:  5; ED:  5; DayOffset: lotDecOne),
    (SY: 1190; SM:  7; SD:  4; EY: 1190; EM:  8; ED:  2; DayOffset: lotDecOne),
    (SY: 1190; SM:  9; SD:  1; EY: 1190; EM:  9; ED: 30; DayOffset: lotDecOne),
    (SY: 1190; SM: 10; SD: 30; EY: 1190; EM: 11; ED: 28; DayOffset: lotDecOne),
    (SY: 1190; SM: 12; SD: 28; EY: 1191; EM:  1; ED: 26; DayOffset: lotDecOne),
    (SY: 1191; SM:  2; SD: 25; EY: 1191; EM:  3; ED: 26; DayOffset: lotDecOne),
    (SY: 1191; SM:  6; SD: 23; EY: 1191; EM:  7; ED: 22; DayOffset: lotDecOne),
    (SY: 1191; SM:  9; SD: 20; EY: 1191; EM: 10; ED: 19; DayOffset: lotDecOne),
    (SY: 1191; SM: 11; SD: 18; EY: 1191; EM: 12; ED: 17; DayOffset: lotDecOne),
    (SY: 1192; SM:  3; SD: 15; EY: 1192; EM:  4; ED: 13; DayOffset: lotDecOne),
    (SY: 1192; SM:  9; SD:  8; EY: 1192; EM: 10; ED:  7; DayOffset: lotDecOne),
    (SY: 1192; SM: 12; SD:  6; EY: 1193; EM:  1; ED:  4; DayOffset: lotDecOne),
    (SY: 1193; SM:  4; SD:  3; EY: 1193; EM:  5; ED:  2; DayOffset: lotDecOne),
    (SY: 1193; SM:  6; SD: 30; EY: 1193; EM:  7; ED: 29; DayOffset: lotDecOne),
    (SY: 1193; SM:  8; SD: 28; EY: 1193; EM:  9; ED: 26; DayOffset: lotDecOne),
    (SY: 1193; SM: 12; SD: 25; EY: 1194; EM:  1; ED: 23; DayOffset: lotDecOne),
    (SY: 1194; SM:  4; SD: 22; EY: 1194; EM:  5; ED: 21; DayOffset: lotDecOne),
    (SY: 1194; SM:  7; SD: 19; EY: 1194; EM:  8; ED: 17; DayOffset: lotDecOne),
    (SY: 1194; SM: 12; SD: 14; EY: 1195; EM:  1; ED: 12; DayOffset: lotDecOne),
    (SY: 1195; SM:  5; SD: 11; EY: 1195; EM:  6; ED:  9; DayOffset: lotDecOne),
    (SY: 1195; SM:  8; SD:  7; EY: 1195; EM:  9; ED:  5; DayOffset: lotDecOne),
    (SY: 1195; SM: 12; SD:  3; EY: 1196; EM:  1; ED:  1; DayOffset: lotDecOne),
    (SY: 1196; SM:  3; SD: 31; EY: 1196; EM:  4; ED: 29; DayOffset: lotDecOne),
    (SY: 1196; SM:  5; SD: 29; EY: 1196; EM:  6; ED: 27; DayOffset: lotDecOne),
    (SY: 1196; SM:  8; SD: 25; EY: 1196; EM:  9; ED: 23; DayOffset: lotDecOne),
    (SY: 1196; SM: 12; SD: 21; EY: 1197; EM:  1; ED: 19; DayOffset: lotDecOne),
    (SY: 1197; SM:  3; SD: 20; EY: 1197; EM:  4; ED: 18; DayOffset: lotDecOne),
    (SY: 1197; SM:  9; SD: 13; EY: 1197; EM: 10; ED: 12; DayOffset: lotDecOne),
    (SY: 1198; SM:  3; SD:  9; EY: 1198; EM:  4; ED:  7; DayOffset: lotDecOne),
    (SY: 1198; SM:  6; SD:  6; EY: 1198; EM:  7; ED:  5; DayOffset: lotDecOne),
    (SY: 1198; SM: 10; SD:  2; EY: 1198; EM: 10; ED: 31; DayOffset: lotDecOne),
    (SY: 1199; SM:  3; SD: 28; EY: 1199; EM:  4; ED: 26; DayOffset: lotDecOne),
    (SY: 1199; SM:  5; SD: 26; EY: 1199; EM:  6; ED: 24; DayOffset: lotDecOne),
    (SY: 1199; SM:  8; SD: 23; EY: 1199; EM:  9; ED: 21; DayOffset: lotDecOne),
    (SY: 1199; SM: 10; SD: 21; EY: 1199; EM: 11; ED: 19; DayOffset: lotDecOne),
    (SY: 1199; SM: 12; SD: 19; EY: 1200; EM:  1; ED: 17; DayOffset: lotDecOne),

    (SY: 1200; SM:  8; SD: 11; EY: 1200; EM:  9; ED:  9; DayOffset: lotDecOne),
    (SY: 1200; SM: 11; SD:  8; EY: 1200; EM: 12; ED:  7; DayOffset: lotDecOne),
    (SY: 1201; SM:  1; SD:  6; EY: 1201; EM:  2; ED:  4; DayOffset: lotDecOne),
    (SY: 1201; SM:  3; SD:  6; EY: 1201; EM:  4; ED:  4; DayOffset: lotDecOne),
    (SY: 1201; SM:  6; SD:  2; EY: 1201; EM:  7; ED:  1; DayOffset: lotDecOne),
    (SY: 1201; SM:  7; SD: 31; EY: 1201; EM:  8; ED: 29; DayOffset: lotDecOne),
    (SY: 1201; SM: 11; SD: 27; EY: 1201; EM: 12; ED: 26; DayOffset: lotDecOne),
    (SY: 1202; SM:  3; SD: 25; EY: 1202; EM:  4; ED: 23; DayOffset: lotDecOne),
    (SY: 1202; SM:  6; SD: 21; EY: 1202; EM:  7; ED: 20; DayOffset: lotDecOne),
    (SY: 1202; SM: 11; SD: 16; EY: 1202; EM: 12; ED: 15; DayOffset: lotDecOne),
    (SY: 1203; SM:  4; SD: 13; EY: 1203; EM:  5; ED: 12; DayOffset: lotDecOne),
    (SY: 1203; SM:  7; SD: 10; EY: 1203; EM:  8; ED:  8; DayOffset: lotDecOne),
    (SY: 1203; SM: 11; SD:  5; EY: 1203; EM: 12; ED:  4; DayOffset: lotDecOne),
    (SY: 1204; SM:  3; SD:  3; EY: 1204; EM:  4; ED:  1; DayOffset: lotDecOne),
    (SY: 1204; SM:  5; SD:  1; EY: 1204; EM:  5; ED: 30; DayOffset: lotDecOne),
    (SY: 1204; SM:  7; SD: 28; EY: 1204; EM:  8; ED: 26; DayOffset: lotDecOne),
    (SY: 1205; SM:  2; SD: 20; EY: 1205; EM:  3; ED: 21; DayOffset: lotDecOne),
    (SY: 1205; SM:  8; SD: 16; EY: 1205; EM:  9; ED: 14; DayOffset: lotDecOne),
    (SY: 1206; SM:  2; SD:  9; EY: 1206; EM:  3; ED: 10; DayOffset: lotDecOne),
    (SY: 1206; SM:  9; SD:  4; EY: 1206; EM: 10; ED:  3; DayOffset: lotDecOne),
    (SY: 1207; SM:  2; SD: 28; EY: 1207; EM:  3; ED: 29; DayOffset: lotDecOne),
    (SY: 1207; SM:  5; SD: 28; EY: 1207; EM:  6; ED: 26; DayOffset: lotDecOne),
    (SY: 1207; SM:  7; SD: 26; EY: 1207; EM:  8; ED: 24; DayOffset: lotDecOne),
    (SY: 1207; SM:  9; SD: 23; EY: 1207; EM: 10; ED: 22; DayOffset: lotDecOne),
    (SY: 1207; SM: 11; SD: 21; EY: 1207; EM: 12; ED: 20; DayOffset: lotDecOne),
    (SY: 1208; SM:  5; SD: 16; EY: 1208; EM:  6; ED: 14; DayOffset: lotDecOne),
    (SY: 1208; SM: 10; SD: 11; EY: 1208; EM: 11; ED:  9; DayOffset: lotDecOne),
    (SY: 1208; SM: 12; SD:  9; EY: 1209; EM:  1; ED:  7; DayOffset: lotDecOne),
    (SY: 1209; SM:  5; SD:  5; EY: 1209; EM:  6; ED:  3; DayOffset: lotDecOne),
    (SY: 1209; SM: 12; SD: 28; EY: 1210; EM:  1; ED: 26; DayOffset: lotDecOne),
    (SY: 1210; SM:  2; SD: 25; EY: 1210; EM:  3; ED: 26; DayOffset: lotDecOne),
    (SY: 1210; SM:  5; SD: 24; EY: 1210; EM:  6; ED: 22; DayOffset: lotDecOne),
    (SY: 1210; SM:  7; SD: 22; EY: 1210; EM:  8; ED: 20; DayOffset: lotDecOne),
    (SY: 1210; SM: 10; SD: 19; EY: 1210; EM: 11; ED: 17; DayOffset: lotDecOne),
    (SY: 1211; SM:  1; SD: 16; EY: 1211; EM:  2; ED: 14; DayOffset: lotDecOne),
    (SY: 1211; SM:  3; SD: 16; EY: 1211; EM:  4; ED: 14; DayOffset: lotDecOne),
    (SY: 1211; SM:  6; SD: 12; EY: 1211; EM:  7; ED: 11; DayOffset: lotDecOne),
    (SY: 1211; SM: 10; SD:  8; EY: 1211; EM: 11; ED:  6; DayOffset: lotDecOne),
    (SY: 1212; SM:  2; SD:  4; EY: 1212; EM:  3; ED:  4; DayOffset: lotDecOne),
    (SY: 1212; SM:  4; SD:  3; EY: 1212; EM:  5; ED:  2; DayOffset: lotDecOne),
    (SY: 1212; SM:  6; SD: 30; EY: 1212; EM:  7; ED: 29; DayOffset: lotDecOne),
    (SY: 1212; SM:  9; SD: 26; EY: 1212; EM: 10; ED: 25; DayOffset: lotDecOne),
    (SY: 1213; SM:  4; SD: 22; EY: 1213; EM:  5; ED: 21; DayOffset: lotDecOne),
    (SY: 1213; SM:  7; SD: 19; EY: 1213; EM:  8; ED: 17; DayOffset: lotDecOne),
    (SY: 1213; SM: 10; SD: 15; EY: 1213; EM: 11; ED: 13; DayOffset: lotDecOne),
    (SY: 1214; SM:  1; SD: 12; EY: 1214; EM:  3; ED: 12; DayOffset: lotDecOne),
    (SY: 1214; SM:  8; SD:  7; EY: 1214; EM:  9; ED:  5; DayOffset: lotDecOne),
    (SY: 1214; SM: 11; SD:  3; EY: 1214; EM: 12; ED:  2; DayOffset: lotDecOne),
    (SY: 1215; SM:  1; SD: 31; EY: 1215; EM:  3; ED:  1; DayOffset: lotDecOne),
    (SY: 1215; SM:  4; SD: 30; EY: 1215; EM:  5; ED: 29; DayOffset: lotDecOne),
    (SY: 1215; SM:  6; SD: 28; EY: 1215; EM:  7; ED: 27; DayOffset: lotDecOne),
    (SY: 1215; SM:  8; SD: 26; EY: 1215; EM:  9; ED: 24; DayOffset: lotDecOne),
    (SY: 1216; SM:  4; SD: 18; EY: 1216; EM:  5; ED: 17; DayOffset: lotDecOne),
    (SY: 1216; SM:  7; SD: 16; EY: 1216; EM:  8; ED: 14; DayOffset: lotDecOne),
    (SY: 1216; SM:  9; SD: 13; EY: 1216; EM: 10; ED: 12; DayOffset: lotDecOne),
    (SY: 1217; SM:  7; SD:  5; EY: 1217; EM:  8; ED:  3; DayOffset: lotDecOne),
    (SY: 1217; SM: 11; SD: 30; EY: 1217; EM: 12; ED: 29; DayOffset: lotDecOne),
    (SY: 1218; SM:  4; SD: 26; EY: 1218; EM:  5; ED: 25; DayOffset: lotDecOne),
    (SY: 1218; SM:  6; SD: 24; EY: 1218; EM:  7; ED: 23; DayOffset: lotDecOne),
    (SY: 1218; SM:  9; SD: 21; EY: 1218; EM: 10; ED: 20; DayOffset: lotDecOne),
    (SY: 1218; SM: 12; SD: 19; EY: 1219; EM:  1; ED: 17; DayOffset: lotDecOne),
    (SY: 1219; SM:  2; SD: 16; EY: 1219; EM:  3; ED: 17; DayOffset: lotDecOne),
    (SY: 1219; SM:  5; SD: 15; EY: 1219; EM:  6; ED: 13; DayOffset: lotDecOne),
    (SY: 1219; SM:  9; SD: 10; EY: 1219; EM: 10; ED:  9; DayOffset: lotDecOne),
    (SY: 1220; SM:  1; SD:  7; EY: 1220; EM:  2; ED:  5; DayOffset: lotDecOne),
    (SY: 1220; SM:  3; SD:  6; EY: 1220; EM:  4; ED:  4; DayOffset: lotDecOne),
    (SY: 1220; SM:  6; SD:  2; EY: 1220; EM:  7; ED:  1; DayOffset: lotDecOne),
    (SY: 1221; SM:  3; SD: 25; EY: 1221; EM:  4; ED: 23; DayOffset: lotDecOne),
    (SY: 1221; SM:  6; SD: 21; EY: 1221; EM:  7; ED: 20; DayOffset: lotDecOne),
    (SY: 1221; SM:  9; SD: 17; EY: 1221; EM: 10; ED: 16; DayOffset: lotDecOne),
    (SY: 1221; SM: 12; SD: 15; EY: 1222; EM:  2; ED: 12; DayOffset: lotDecOne),
    (SY: 1222; SM:  4; SD: 13; EY: 1222; EM:  5; ED: 12; DayOffset: lotDecOne),
    (SY: 1222; SM:  7; SD: 10; EY: 1222; EM:  8; ED:  8; DayOffset: lotDecOne),
    (SY: 1222; SM: 10; SD:  6; EY: 1222; EM: 11; ED:  4; DayOffset: lotDecOne),
    (SY: 1223; SM:  1; SD:  3; EY: 1223; EM:  2; ED:  1; DayOffset: lotDecOne),
    (SY: 1223; SM:  7; SD: 29; EY: 1223; EM:  8; ED: 27; DayOffset: lotDecOne),
    (SY: 1223; SM: 10; SD: 25; EY: 1223; EM: 11; ED: 23; DayOffset: lotDecOne),
    (SY: 1223; SM: 12; SD: 23; EY: 1224; EM:  1; ED: 21; DayOffset: lotDecOne),
    (SY: 1224; SM:  6; SD: 18; EY: 1224; EM:  7; ED: 17; DayOffset: lotDecOne),
    (SY: 1224; SM:  8; SD: 16; EY: 1224; EM:  9; ED: 14; DayOffset: lotDecOne),
    (SY: 1224; SM: 11; SD: 12; EY: 1224; EM: 12; ED: 11; DayOffset: lotDecOne),
    (SY: 1225; SM:  1; SD: 10; EY: 1225; EM:  2; ED:  8; DayOffset: lotDecOne),
    (SY: 1226; SM:  1; SD: 29; EY: 1226; EM:  2; ED: 27; DayOffset: lotDecOne),
    (SY: 1226; SM:  3; SD: 29; EY: 1226; EM:  4; ED: 27; DayOffset: lotDecOne),
    (SY: 1226; SM:  8; SD: 24; EY: 1226; EM:  9; ED: 22; DayOffset: lotDecOne),
    (SY: 1227; SM:  4; SD: 17; EY: 1227; EM:  5; ED: 16; DayOffset: lotDecOne),
    (SY: 1227; SM:  6; SD: 15; EY: 1227; EM:  7; ED: 14; DayOffset: lotDecOne),
    (SY: 1228; SM:  2; SD:  7; EY: 1228; EM:  3; ED:  7; DayOffset: lotDecOne),
    (SY: 1228; SM:  5; SD:  5; EY: 1228; EM:  6; ED:  3; DayOffset: lotDecOne),
    (SY: 1228; SM: 11; SD: 28; EY: 1228; EM: 12; ED: 27; DayOffset: lotDecOne),
    (SY: 1229; SM:  2; SD: 25; EY: 1229; EM:  3; ED: 26; DayOffset: lotDecOne),
    (SY: 1229; SM:  5; SD: 24; EY: 1229; EM:  6; ED: 22; DayOffset: lotDecOne),
    (SY: 1229; SM:  8; SD: 20; EY: 1229; EM:  9; ED: 18; DayOffset: lotDecOne),
    (SY: 1229; SM: 11; SD: 17; EY: 1230; EM:  1; ED: 15; DayOffset: lotDecOne),
    (SY: 1230; SM:  3; SD: 16; EY: 1230; EM:  4; ED: 14; DayOffset: lotDecOne),
    (SY: 1230; SM:  6; SD: 12; EY: 1230; EM:  7; ED: 11; DayOffset: lotDecOne),
    (SY: 1230; SM:  9; SD:  8; EY: 1230; EM: 10; ED:  7; DayOffset: lotDecOne),
    (SY: 1230; SM: 11; SD:  6; EY: 1230; EM: 12; ED:  5; DayOffset: lotDecOne),
    (SY: 1231; SM:  7; SD:  1; EY: 1231; EM:  7; ED: 30; DayOffset: lotDecOne),
    (SY: 1231; SM:  9; SD: 27; EY: 1231; EM: 10; ED: 26; DayOffset: lotDecOne),
    (SY: 1231; SM: 11; SD: 25; EY: 1231; EM: 12; ED: 24; DayOffset: lotDecOne),
    (SY: 1232; SM:  3; SD: 23; EY: 1232; EM:  4; ED: 21; DayOffset: lotDecOne),
    (SY: 1232; SM:  7; SD: 19; EY: 1232; EM:  8; ED: 17; DayOffset: lotDecOne),
    (SY: 1232; SM: 10; SD: 15; EY: 1232; EM: 11; ED: 13; DayOffset: lotDecOne),
    (SY: 1232; SM: 12; SD: 13; EY: 1233; EM:  1; ED: 11; DayOffset: lotDecOne),
    (SY: 1233; SM:  3; SD: 12; EY: 1233; EM:  4; ED: 10; DayOffset: lotDecOne),
    (SY: 1233; SM:  6; SD:  9; EY: 1233; EM:  7; ED:  8; DayOffset: lotDecOne),
    (SY: 1233; SM: 11; SD:  3; EY: 1233; EM: 12; ED:  2; DayOffset: lotDecOne),
    (SY: 1234; SM:  1; SD:  1; EY: 1234; EM:  1; ED: 30; DayOffset: lotDecOne),
    (SY: 1234; SM:  3; SD:  1; EY: 1234; EM:  3; ED: 30; DayOffset: lotDecOne),
    (SY: 1234; SM:  5; SD: 29; EY: 1234; EM:  6; ED: 27; DayOffset: lotDecOne),
    (SY: 1235; SM:  1; SD: 20; EY: 1235; EM:  2; ED: 18; DayOffset: lotDecOne),
    (SY: 1235; SM:  3; SD: 20; EY: 1235; EM:  4; ED: 18; DayOffset: lotDecOne),
    (SY: 1235; SM:  5; SD: 18; EY: 1235; EM:  6; ED: 16; DayOffset: lotDecOne),
    (SY: 1235; SM:  8; SD: 15; EY: 1235; EM:  9; ED: 13; DayOffset: lotDecOne),
    (SY: 1236; SM:  4; SD:  7; EY: 1236; EM:  5; ED:  6; DayOffset: lotDecOne),
    (SY: 1236; SM:  8; SD:  3; EY: 1236; EM:  9; ED:  1; DayOffset: lotDecOne),
    (SY: 1236; SM: 10; SD: 31; EY: 1236; EM: 11; ED: 29; DayOffset: lotDecOne),
    (SY: 1237; SM:  4; SD: 26; EY: 1237; EM:  5; ED: 25; DayOffset: lotDecOne),
    (SY: 1237; SM:  7; SD: 23; EY: 1237; EM:  8; ED: 21; DayOffset: lotDecOne),
    (SY: 1237; SM: 10; SD: 20; EY: 1237; EM: 11; ED: 18; DayOffset: lotDecOne),
    (SY: 1238; SM:  5; SD: 15; EY: 1238; EM:  6; ED: 13; DayOffset: lotDecOne),
    (SY: 1238; SM:  8; SD: 11; EY: 1238; EM:  9; ED:  9; DayOffset: lotDecOne),
    (SY: 1238; SM: 10; SD:  9; EY: 1238; EM: 11; ED:  7; DayOffset: lotDecOne),
    (SY: 1239; SM:  6; SD:  3; EY: 1239; EM:  7; ED:  2; DayOffset: lotDecOne),
    (SY: 1239; SM:  8; SD: 30; EY: 1239; EM:  9; ED: 28; DayOffset: lotDecOne),
    (SY: 1239; SM: 10; SD: 28; EY: 1239; EM: 11; ED: 26; DayOffset: lotDecOne),
    (SY: 1240; SM:  2; SD: 24; EY: 1240; EM:  3; ED: 24; DayOffset: lotDecOne),
    (SY: 1240; SM:  6; SD: 21; EY: 1240; EM:  7; ED: 20; DayOffset: lotDecOne),
    (SY: 1240; SM:  9; SD: 17; EY: 1240; EM: 10; ED: 16; DayOffset: lotDecOne),
    (SY: 1241; SM:  2; SD: 12; EY: 1241; EM:  3; ED: 13; DayOffset: lotDecOne),
    (SY: 1241; SM:  5; SD: 12; EY: 1241; EM:  6; ED: 10; DayOffset: lotDecOne),
    (SY: 1241; SM:  7; SD: 10; EY: 1241; EM:  8; ED:  8; DayOffset: lotDecOne),
    (SY: 1241; SM: 10; SD:  6; EY: 1241; EM: 11; ED:  4; DayOffset: lotDecOne),
    (SY: 1242; SM:  2; SD:  1; EY: 1242; EM:  3; ED:  2; DayOffset: lotDecOne),
    (SY: 1242; SM: 10; SD: 25; EY: 1242; EM: 11; ED: 23; DayOffset: lotDecOne),
    (SY: 1242; SM: 12; SD: 23; EY: 1243; EM:  1; ED: 21; DayOffset: lotDecOne),
    (SY: 1243; SM:  2; SD: 20; EY: 1243; EM:  3; ED: 21; DayOffset: lotDecOne),
    (SY: 1243; SM:  7; SD: 18; EY: 1243; EM:  8; ED: 16; DayOffset: lotDecOne),
    (SY: 1244; SM:  1; SD: 11; EY: 1244; EM:  2; ED:  9; DayOffset: lotDecOne),
    (SY: 1244; SM:  3; SD: 10; EY: 1244; EM:  4; ED:  8; DayOffset: lotDecOne),
    (SY: 1244; SM:  5; SD:  8; EY: 1244; EM:  6; ED:  6; DayOffset: lotDecOne),
    (SY: 1244; SM:  7; SD:  6; EY: 1244; EM:  8; ED:  4; DayOffset: lotDecOne),
    (SY: 1244; SM: 10; SD:  3; EY: 1244; EM: 11; ED:  1; DayOffset: lotDecOne),
    (SY: 1245; SM:  3; SD: 29; EY: 1245; EM:  4; ED: 27; DayOffset: lotDecOne),
    (SY: 1246; SM:  4; SD: 17; EY: 1246; EM:  5; ED: 16; DayOffset: lotDecOne),
    (SY: 1246; SM:  7; SD: 14; EY: 1246; EM:  8; ED: 12; DayOffset: lotDecOne),
    (SY: 1247; SM:  5; SD:  6; EY: 1247; EM:  6; ED:  4; DayOffset: lotDecOne),
    (SY: 1247; SM:  8; SD:  2; EY: 1247; EM:  8; ED: 31; DayOffset: lotDecOne),
    (SY: 1247; SM:  9; SD: 30; EY: 1247; EM: 10; ED: 29; DayOffset: lotDecOne),
    (SY: 1248; SM:  1; SD: 27; EY: 1248; EM:  2; ED: 25; DayOffset: lotDecOne),
    (SY: 1248; SM:  5; SD: 24; EY: 1248; EM:  6; ED: 22; DayOffset: lotDecOne),
    (SY: 1248; SM:  8; SD: 20; EY: 1248; EM:  9; ED: 18; DayOffset: lotDecOne),
    (SY: 1249; SM:  1; SD: 15; EY: 1249; EM:  2; ED: 13; DayOffset: lotDecOne),
    (SY: 1249; SM:  4; SD: 14; EY: 1249; EM:  5; ED: 13; DayOffset: lotDecOne),
    (SY: 1249; SM:  6; SD: 12; EY: 1249; EM:  7; ED: 11; DayOffset: lotDecOne),
    (SY: 1249; SM:  9; SD:  8; EY: 1249; EM: 10; ED:  7; DayOffset: lotDecOne),
    (SY: 1250; SM:  1; SD:  4; EY: 1250; EM:  2; ED:  2; DayOffset: lotDecOne),
    (SY: 1250; SM:  5; SD:  3; EY: 1250; EM:  6; ED:  1; DayOffset: lotDecOne),
    (SY: 1250; SM:  9; SD: 27; EY: 1250; EM: 10; ED: 26; DayOffset: lotDecOne),
    (SY: 1251; SM:  1; SD: 23; EY: 1251; EM:  2; ED: 21; DayOffset: lotDecOne),
    (SY: 1251; SM:  4; SD: 22; EY: 1251; EM:  5; ED: 21; DayOffset: lotDecOne),
    (SY: 1251; SM:  7; SD: 20; EY: 1251; EM:  8; ED: 18; DayOffset: lotDecOne),
    (SY: 1251; SM: 10; SD: 16; EY: 1251; EM: 11; ED: 14; DayOffset: lotDecOne),
    (SY: 1252; SM:  2; SD: 11; EY: 1252; EM:  3; ED: 11; DayOffset: lotDecOne),
    (SY: 1252; SM:  4; SD: 10; EY: 1252; EM:  5; ED:  9; DayOffset: lotDecOne),
    (SY: 1252; SM:  9; SD:  5; EY: 1252; EM: 10; ED:  4; DayOffset: lotDecOne),
    (SY: 1252; SM: 11; SD:  3; EY: 1252; EM: 12; ED:  2; DayOffset: lotDecOne),
    (SY: 1253; SM:  1; SD:  1; EY: 1253; EM:  1; ED: 30; DayOffset: lotDecOne),
    (SY: 1253; SM:  6; SD: 27; EY: 1253; EM:  7; ED: 26; DayOffset: lotDecOne),
    (SY: 1253; SM:  9; SD: 24; EY: 1253; EM: 10; ED: 23; DayOffset: lotDecOne),
    (SY: 1253; SM: 11; SD: 22; EY: 1253; EM: 12; ED: 21; DayOffset: lotDecOne),
    (SY: 1254; SM:  1; SD: 20; EY: 1254; EM:  2; ED: 18; DayOffset: lotDecOne),
    (SY: 1254; SM:  3; SD: 20; EY: 1254; EM:  4; ED: 18; DayOffset: lotDecOne),
    (SY: 1254; SM:  6; SD: 16; EY: 1254; EM:  7; ED: 15; DayOffset: lotDecOne),
    (SY: 1254; SM:  9; SD: 13; EY: 1254; EM: 10; ED: 12; DayOffset: lotDecOne),
    (SY: 1254; SM: 12; SD: 11; EY: 1255; EM:  1; ED:  9; DayOffset: lotDecOne),
    (SY: 1255; SM:  4; SD:  8; EY: 1255; EM:  5; ED:  7; DayOffset: lotDecOne),
    (SY: 1255; SM:  7; SD:  5; EY: 1255; EM:  8; ED:  3; DayOffset: lotDecOne),
    (SY: 1255; SM:  9; SD:  2; EY: 1255; EM: 10; ED:  1; DayOffset: lotDecOne),
    (SY: 1255; SM: 12; SD: 30; EY: 1256; EM:  1; ED: 28; DayOffset: lotDecOne),
    (SY: 1256; SM:  4; SD: 26; EY: 1256; EM:  5; ED: 25; DayOffset: lotDecOne),
    (SY: 1256; SM:  7; SD: 23; EY: 1256; EM:  8; ED: 21; DayOffset: lotDecOne),
    (SY: 1256; SM: 12; SD: 18; EY: 1257; EM:  1; ED: 16; DayOffset: lotDecOne),
    (SY: 1257; SM:  3; SD: 17; EY: 1257; EM:  4; ED: 15; DayOffset: lotDecOne),
    (SY: 1257; SM:  5; SD: 15; EY: 1257; EM:  6; ED: 13; DayOffset: lotDecOne),
    (SY: 1257; SM:  8; SD: 11; EY: 1257; EM:  9; ED:  9; DayOffset: lotDecOne),
    (SY: 1257; SM: 12; SD:  7; EY: 1258; EM:  1; ED:  5; DayOffset: lotDecOne),
    (SY: 1258; SM:  4; SD:  5; EY: 1258; EM:  5; ED:  4; DayOffset: lotDecOne),
    (SY: 1258; SM:  6; SD:  3; EY: 1258; EM:  7; ED:  2; DayOffset: lotDecOne),
    (SY: 1258; SM:  8; SD: 30; EY: 1258; EM:  9; ED: 28; DayOffset: lotDecOne),
    (SY: 1258; SM: 12; SD: 26; EY: 1259; EM:  1; ED: 24; DayOffset: lotDecOne),
    (SY: 1259; SM:  3; SD: 25; EY: 1259; EM:  4; ED: 23; DayOffset: lotDecOne),
    (SY: 1259; SM:  9; SD: 18; EY: 1259; EM: 10; ED: 17; DayOffset: lotDecOne),
    (SY: 1260; SM:  3; SD: 13; EY: 1260; EM:  4; ED: 11; DayOffset: lotDecOne),
    (SY: 1260; SM:  6; SD: 10; EY: 1260; EM:  7; ED:  9; DayOffset: lotDecOne),
    (SY: 1260; SM:  8; SD:  8; EY: 1260; EM:  9; ED:  6; DayOffset: lotDecOne),
    (SY: 1260; SM: 10; SD:  6; EY: 1260; EM: 11; ED:  4; DayOffset: lotDecOne),
    (SY: 1261; SM:  5; SD: 30; EY: 1261; EM:  6; ED: 28; DayOffset: lotDecOne),
    (SY: 1261; SM:  8; SD: 27; EY: 1261; EM:  9; ED: 25; DayOffset: lotDecOne),
    (SY: 1261; SM: 10; SD: 25; EY: 1261; EM: 11; ED: 23; DayOffset: lotDecOne),
    (SY: 1261; SM: 12; SD: 23; EY: 1262; EM:  1; ED: 21; DayOffset: lotDecOne),
    (SY: 1262; SM:  8; SD: 16; EY: 1262; EM:  9; ED: 14; DayOffset: lotDecOne),
    (SY: 1262; SM: 11; SD: 13; EY: 1262; EM: 12; ED: 12; DayOffset: lotDecOne),
    (SY: 1263; SM:  1; SD: 11; EY: 1263; EM:  2; ED:  9; DayOffset: lotDecOne),
    (SY: 1263; SM:  6; SD:  7; EY: 1263; EM:  7; ED:  6; DayOffset: lotDecOne),
    (SY: 1263; SM:  8; SD:  5; EY: 1263; EM:  9; ED:  3; DayOffset: lotDecOne),
    (SY: 1263; SM: 11; SD:  2; EY: 1263; EM: 12; ED:  1; DayOffset: lotIncOne),
    (SY: 1264; SM:  1; SD: 30; EY: 1264; EM:  2; ED: 28; DayOffset: lotDecOne),
    (SY: 1264; SM:  3; SD: 29; EY: 1264; EM:  4; ED: 27; DayOffset: lotDecOne),
    (SY: 1264; SM:  6; SD: 25; EY: 1264; EM:  7; ED: 24; DayOffset: lotDecOne),
    (SY: 1264; SM: 11; SD: 20; EY: 1264; EM: 12; ED: 19; DayOffset: lotDecOne),
    (SY: 1265; SM:  2; SD: 17; EY: 1265; EM:  3; ED: 18; DayOffset: lotDecOne),
    (SY: 1265; SM:  4; SD: 17; EY: 1265; EM:  5; ED: 16; DayOffset: lotDecOne),
    (SY: 1265; SM:  7; SD: 14; EY: 1265; EM:  8; ED: 12; DayOffset: lotDecOne),
    (SY: 1265; SM: 11; SD:  9; EY: 1265; EM: 12; ED:  8; DayOffset: lotDecOne),
    (SY: 1266; SM:  3; SD:  8; EY: 1266; EM:  4; ED:  6; DayOffset: lotDecOne),
    (SY: 1266; SM:  5; SD:  6; EY: 1266; EM:  6; ED:  4; DayOffset: lotDecOne),
    (SY: 1266; SM:  8; SD:  2; EY: 1266; EM:  8; ED: 31; DayOffset: lotDecOne),
    (SY: 1267; SM:  2; SD: 25; EY: 1267; EM:  3; ED: 26; DayOffset: lotDecOne),
    (SY: 1267; SM:  8; SD: 21; EY: 1267; EM:  9; ED: 19; DayOffset: lotDecOne),
    (SY: 1267; SM: 11; SD: 17; EY: 1267; EM: 12; ED: 16; DayOffset: lotDecOne),
    (SY: 1268; SM:  2; SD: 14; EY: 1268; EM:  3; ED: 14; DayOffset: lotDecOne),
    (SY: 1268; SM:  9; SD:  8; EY: 1268; EM: 10; ED:  7; DayOffset: lotDecOne),
    (SY: 1269; SM:  3; SD:  4; EY: 1269; EM:  4; ED:  2; DayOffset: lotDecOne),
    (SY: 1269; SM:  6; SD:  1; EY: 1269; EM:  6; ED: 30; DayOffset: lotDecOne),
    (SY: 1269; SM:  7; SD: 30; EY: 1269; EM:  8; ED: 28; DayOffset: lotDecOne),
    (SY: 1269; SM:  9; SD: 27; EY: 1269; EM: 10; ED: 26; DayOffset: lotDecOne),
    (SY: 1270; SM:  5; SD: 21; EY: 1270; EM:  6; ED: 19; DayOffset: lotDecOne),
    (SY: 1270; SM: 10; SD: 16; EY: 1270; EM: 11; ED: 14; DayOffset: lotDecOne),
    (SY: 1271; SM:  5; SD: 10; EY: 1271; EM:  6; ED:  8; DayOffset: lotDecOne),
    (SY: 1272; SM:  1; SD:  2; EY: 1272; EM:  1; ED: 31; DayOffset: lotDecOne),
    (SY: 1272; SM:  3; SD:  1; EY: 1272; EM:  3; ED: 30; DayOffset: lotDecOne),
    (SY: 1272; SM:  5; SD: 28; EY: 1272; EM:  6; ED: 26; DayOffset: lotDecOne),
    (SY: 1272; SM:  7; SD: 26; EY: 1272; EM:  8; ED: 24; DayOffset: lotDecOne),
    (SY: 1272; SM: 10; SD: 23; EY: 1272; EM: 11; ED: 21; DayOffset: lotDecOne),
    (SY: 1273; SM:  1; SD: 20; EY: 1273; EM:  2; ED: 18; DayOffset: lotDecOne),
    (SY: 1273; SM:  3; SD: 20; EY: 1273; EM:  4; ED: 18; DayOffset: lotDecOne),
    (SY: 1273; SM:  6; SD: 16; EY: 1273; EM:  7; ED: 15; DayOffset: lotDecOne),
    (SY: 1273; SM: 10; SD: 12; EY: 1273; EM: 11; ED: 10; DayOffset: lotDecOne),
    (SY: 1274; SM:  2; SD:  8; EY: 1274; EM:  3; ED:  9; DayOffset: lotDecOne),
    (SY: 1274; SM:  4; SD:  8; EY: 1274; EM:  5; ED:  7; DayOffset: lotDecOne),
    (SY: 1274; SM:  7; SD:  5; EY: 1274; EM:  8; ED:  3; DayOffset: lotDecOne),
    (SY: 1274; SM: 10; SD:  1; EY: 1274; EM: 10; ED: 30; DayOffset: lotDecOne),
    (SY: 1275; SM:  4; SD: 27; EY: 1275; EM:  5; ED: 26; DayOffset: lotDecOne),
    (SY: 1275; SM:  7; SD: 24; EY: 1275; EM:  8; ED: 22; DayOffset: lotDecOne),
    (SY: 1275; SM: 10; SD: 20; EY: 1275; EM: 11; ED: 18; DayOffset: lotDecOne),
    (SY: 1276; SM:  1; SD: 17; EY: 1276; EM:  3; ED: 16; DayOffset: lotDecOne),
    (SY: 1276; SM:  8; SD: 11; EY: 1276; EM:  9; ED:  9; DayOffset: lotDecOne),

    (SY: 1276; SM: 11; SD:  7; EY: 1276; EM: 12; ED:  6; DayOffset: lotDecOne),
    (SY: 1277; SM:  2; SD:  4; EY: 1277; EM:  3; ED:  5; DayOffset: lotDecOne),
    (SY: 1277; SM:  5; SD:  4; EY: 1277; EM:  6; ED:  2; DayOffset: lotDecOne),
    (SY: 1277; SM:  7; SD:  2; EY: 1277; EM:  7; ED: 31; DayOffset: lotDecOne),
    (SY: 1277; SM:  8; SD: 30; EY: 1277; EM:  9; ED: 28; DayOffset: lotDecOne),
    (SY: 1278; SM:  4; SD: 23; EY: 1278; EM:  5; ED: 22; DayOffset: lotDecOne),
    (SY: 1278; SM:  9; SD: 18; EY: 1278; EM: 10; ED: 17; DayOffset: lotDecOne),
    (SY: 1279; SM:  7; SD: 10; EY: 1279; EM:  8; ED:  8; DayOffset: lotDecOne),
    (SY: 1279; SM: 12; SD:  5; EY: 1280; EM:  1; ED:  3; DayOffset: lotDecOne),
    (SY: 1280; SM:  4; SD: 30; EY: 1280; EM:  5; ED: 29; DayOffset: lotDecOne),
    (SY: 1280; SM:  6; SD: 28; EY: 1280; EM:  7; ED: 27; DayOffset: lotDecOne),
    (SY: 1280; SM:  9; SD: 25; EY: 1280; EM: 10; ED: 24; DayOffset: lotDecOne),
    (SY: 1280; SM: 12; SD: 23; EY: 1281; EM:  1; ED: 21; DayOffset: lotDecOne),
    (SY: 1281; SM:  3; SD: 21; EY: 1281; EM:  4; ED: 19; DayOffset: lotIncOne),
    (SY: 1282; SM: 12; SD: 31; EY: 1283; EM:  1; ED: 29; DayOffset: lotDecOne),
    (SY: 1284; SM: 12; SD:  8; EY: 1285; EM:  1; ED:  6; DayOffset: lotIncOne),
    (SY: 1287; SM: 12; SD:  6; EY: 1288; EM:  1; ED:  4; DayOffset: lotIncOne),
    (SY: 1297; SM: 10; SD: 17; EY: 1297; EM: 11; ED: 15; DayOffset: lotIncOne),

    (SY: 1300; SM: 10; SD: 13; EY: 1300; EM: 11; ED: 12; DayOffset: lotDecOne),
    (SY: 1300; SM: 11; SD: 13; EY: 1300; EM: 12; ED: 11; DayOffset: lotDecOne),
    (SY: 1307; SM:  6; SD: 30; EY: 1307; EM:  7; ED: 29; DayOffset: lotIncOne),
    (SY: 1313; SM:  6; SD: 24; EY: 1313; EM:  7; ED: 23; DayOffset: lotIncOne),
    (SY: 1317; SM:  2; SD: 12; EY: 1317; EM:  3; ED: 13; DayOffset: lotDecOne),
    (SY: 1318; SM: 11; SD: 23; EY: 1318; EM: 12; ED: 22; DayOffset: lotIncOne),
    (SY: 1319; SM:  6; SD: 18; EY: 1319; EM:  7; ED: 17; DayOffset: lotIncOne),
    (SY: 1321; SM:  7; SD: 25; EY: 1321; EM:  8; ED: 23; DayOffset: lotIncOne),
    (SY: 1324; SM:  5; SD: 23; EY: 1324; EM:  6; ED: 21; DayOffset: lotIncOne),
    (SY: 1326; SM: 10; SD: 26; EY: 1326; EM: 11; ED: 24; DayOffset: lotIncOne),
    (SY: 1330; SM: 12; SD: 10; EY: 1331; EM:  1; ED:  8; DayOffset: lotIncOne),
    (SY: 1333; SM:  7; SD: 12; EY: 1333; EM:  8; ED: 10; DayOffset: lotIncOne),
    (SY: 1335; SM:  8; SD: 19; EY: 1335; EM:  9; ED: 17; DayOffset: lotDecOne),
    (SY: 1337; SM:  8; SD: 26; EY: 1337; EM:  9; ED: 24; DayOffset: lotIncOne),
    (SY: 1339; SM: 10; SD:  3; EY: 1339; EM: 11; ED:  1; DayOffset: lotIncOne),
    (SY: 1344; SM:  7; SD: 10; EY: 1344; EM:  8; ED:  8; DayOffset: lotIncOne),
    (SY: 1352; SM:  8; SD: 10; EY: 1352; EM:  9; ED:  8; DayOffset: lotIncOne),
    (SY: 1353; SM:  7; SD:  1; EY: 1353; EM:  7; ED: 30; DayOffset: lotIncOne),
    (SY: 1362; SM:  6; SD: 22; EY: 1362; EM:  7; ED: 21; DayOffset: lotIncOne),
    (SY: 1366; SM:  9; SD:  5; EY: 1366; EM: 10; ED:  4; DayOffset: lotIncOne),
    (SY: 1371; SM:  6; SD: 13; EY: 1371; EM:  7; ED: 12; DayOffset: lotIncOne),
    (SY: 1372; SM:  7; SD:  1; EY: 1372; EM:  7; ED: 30; DayOffset: lotIncOne),
    (SY: 1373; SM:  7; SD: 20; EY: 1373; EM:  8; ED: 18; DayOffset: lotIncOne),
    (SY: 1380; SM: 11; SD: 27; EY: 1380; EM: 12; ED: 26; DayOffset: lotIncOne),
    (SY: 1381; SM:  9; SD: 18; EY: 1381; EM: 10; ED: 17; DayOffset: lotIncOne),
    (SY: 1382; SM:  7; SD: 11; EY: 1382; EM:  8; ED:  9; DayOffset: lotIncOne),
    (SY: 1388; SM: 10; SD: 30; EY: 1388; EM: 11; ED: 28; DayOffset: lotIncOne),
    (SY: 1393; SM:  5; SD: 11; EY: 1393; EM:  6; ED:  9; DayOffset: lotIncOne),
    (SY: 1397; SM:  7; SD: 24; EY: 1397; EM:  8; ED: 22; DayOffset: lotIncOne),

    (SY: 1411; SM:  3; SD: 24; EY: 1411; EM:  4; ED: 22; DayOffset: lotIncOne),
    (SY: 1414; SM:  6; SD: 17; EY: 1414; EM:  7; ED: 16; DayOffset: lotIncOne),
    (SY: 1420; SM: 10; SD:  7; EY: 1420; EM: 11; ED:  5; DayOffset: lotIncOne),
    (SY: 1421; SM:  7; SD: 29; EY: 1421; EM:  8; ED: 27; DayOffset: lotIncOne),
    (SY: 1432; SM:  1; SD:  3; EY: 1432; EM:  2; ED:  1; DayOffset: lotDecOne),
    (SY: 1440; SM:  8; SD: 27; EY: 1440; EM:  9; ED: 25; DayOffset: lotIncOne),
    (SY: 1442; SM:  9; SD:  4; EY: 1442; EM: 10; ED:  3; DayOffset: lotIncOne),
    (SY: 1449; SM: 11; SD: 15; EY: 1449; EM: 12; ED: 14; DayOffset: lotIncOne),
    (SY: 1458; SM: 10; SD:  7; EY: 1458; EM: 11; ED:  5; DayOffset: lotIncOne),
    (SY: 1462; SM: 11; SD: 21; EY: 1462; EM: 12; ED: 20; DayOffset: lotDecOne),
    (SY: 1464; SM: 12; SD: 28; EY: 1465; EM:  1; ED: 26; DayOffset: lotDecOne),
    (SY: 1467; SM:  7; SD:  1; EY: 1467; EM:  7; ED: 30; DayOffset: lotIncOne),
    (SY: 1475; SM: 11; SD: 28; EY: 1475; EM: 12; ED: 27; DayOffset: lotIncOne),
    (SY: 1480; SM:  3; SD: 11; EY: 1480; EM:  4; ED:  9; DayOffset: lotIncOne),
    (SY: 1481; SM:  2; SD: 28; EY: 1481; EM:  3; ED: 29; DayOffset: lotIncOne),
    (SY: 1484; SM:  7; SD: 22; EY: 1484; EM:  8; ED: 20; DayOffset: lotIncOne),
    (SY: 1490; SM:  7; SD: 17; EY: 1490; EM:  8; ED: 15; DayOffset: lotIncOne),
    (SY: 1495; SM:  7; SD: 21; EY: 1495; EM:  8; ED: 19; DayOffset: lotIncOne),
    (SY: 1496; SM: 10; SD:  6; EY: 1496; EM: 11; ED:  4; DayOffset: lotIncOne),
    (SY: 1497; SM: 10; SD: 25; EY: 1497; EM: 11; ED: 23; DayOffset: lotDecOne),

    (SY: 1501; SM:  4; SD: 17; EY: 1501; EM:  5; ED: 16; DayOffset: lotIncOne),
    (SY: 1501; SM:  6; SD: 15; EY: 1501; EM:  7; ED: 14; DayOffset: lotIncOne),
    (SY: 1508; SM:  1; SD:  2; EY: 1508; EM:  1; ED: 31; DayOffset: lotDecOne),
    (SY: 1513; SM: 10; SD: 28; EY: 1513; EM: 11; ED: 26; DayOffset: lotIncOne),
    (SY: 1516; SM: 10; SD: 25; EY: 1516; EM: 11; ED: 23; DayOffset: lotDecOne),
    (SY: 1521; SM:  9; SD: 30; EY: 1521; EM: 10; ED: 29; DayOffset: lotIncOne),
    (SY: 1526; SM:  7; SD:  9; EY: 1526; EM:  8; ED:  7; DayOffset: lotIncOne),
    (SY: 1527; SM:  6; SD: 28; EY: 1527; EM:  7; ED: 27; DayOffset: lotIncOne),
    (SY: 1535; SM:  8; SD: 28; EY: 1535; EM:  9; ED: 26; DayOffset: lotIncOne),
    (SY: 1535; SM: 10; SD: 26; EY: 1535; EM: 11; ED: 24; DayOffset: lotDecOne),
    (SY: 1544; SM:  5; SD: 21; EY: 1544; EM:  6; ED: 19; DayOffset: lotIncOne),
    (SY: 1546; SM:  1; SD:  2; EY: 1546; EM:  1; ED: 31; DayOffset: lotDecOne),
    (SY: 1546; SM:  7; SD: 27; EY: 1546; EM:  8; ED: 25; DayOffset: lotIncOne),
    (SY: 1571; SM:  8; SD: 20; EY: 1571; EM:  9; ED: 18; DayOffset: lotIncOne),
    (SY: 1572; SM:  8; SD:  8; EY: 1572; EM:  9; ED:  6; DayOffset: lotIncOne),
    (SY: 1581; SM: 10; SD: 27; EY: 1581; EM: 11; ED: 25; DayOffset: lotDecOne),
    (SY: 1582; SM:  7; SD: 19; EY: 1582; EM:  8; ED: 17; DayOffset: lotIncOne),
    (SY: 1588; SM:  3; SD: 26; EY: 1588; EM:  4; ED: 25; DayOffset: lotIncOne),
    (SY: 1588; SM:  4; SD: 26; EY: 1588; EM:  5; ED: 24; DayOffset: lotIncOne),
    (SY: 1589; SM:  1; SD: 16; EY: 1589; EM:  2; ED: 14; DayOffset: lotDecOne),
    (SY: 1591; SM:  9; SD: 17; EY: 1591; EM: 10; ED: 16; DayOffset: lotIncOne),
    (SY: 1599; SM:  1; SD: 26; EY: 1599; EM:  2; ED: 24; DayOffset: lotDecOne),

    (SY: 1600; SM:  2; SD: 14; EY: 1600; EM:  3; ED: 14; DayOffset: lotIncOne),
    (SY: 1612; SM:  3; SD:  2; EY: 1612; EM:  3; ED: 31; DayOffset: lotDecOne),
    (SY: 1616; SM:  5; SD: 15; EY: 1616; EM:  6; ED: 13; DayOffset: lotIncOne),
    (SY: 1622; SM:  7; SD:  8; EY: 1622; EM:  8; ED:  6; DayOffset: lotIncOne),
    (SY: 1627; SM:  9; SD:  9; EY: 1627; EM: 10; ED:  8; DayOffset: lotIncOne),
    (SY: 1628; SM:  1; SD:  6; EY: 1628; EM:  2; ED:  4; DayOffset: lotDecOne),
    (SY: 1630; SM:  4; SD: 12; EY: 1630; EM:  5; ED: 11; DayOffset: lotDecOne),
    (SY: 1634; SM:  8; SD: 23; EY: 1634; EM:  9; ED: 21; DayOffset: lotIncOne),
    (SY: 1643; SM:  2; SD: 18; EY: 1643; EM:  3; ED: 19; DayOffset: lotDecOne),
    (SY: 1649; SM:  5; SD: 11; EY: 1649; EM:  6; ED:  9; DayOffset: lotIncOne),
    (SY: 1662; SM:  2; SD: 18; EY: 1662; EM:  3; ED: 19; DayOffset: lotIncOne),
    (SY: 1673; SM: 11; SD:  8; EY: 1673; EM: 12; ED:  7; DayOffset: lotDecOne),
    (SY: 1685; SM:  2; SD:  3; EY: 1685; EM:  3; ED:  4; DayOffset: lotIncOne),
    (SY: 1687; SM:  3; SD: 13; EY: 1687; EM:  4; ED: 11; DayOffset: lotIncOne),
    (SY: 1694; SM:  6; SD: 22; EY: 1694; EM:  7; ED: 21; DayOffset: lotIncOne),

    (SY: 1704; SM: 10; SD: 28; EY: 1704; EM: 11; ED: 26; DayOffset: lotDecOne),
    (SY: 1708; SM:  2; SD: 21; EY: 1708; EM:  3; ED: 21; DayOffset: lotIncOne),
    (SY: 1720; SM:  7; SD:  5; EY: 1720; EM:  8; ED:  3; DayOffset: lotIncOne),
    (SY: 1759; SM:  3; SD: 28; EY: 1759; EM:  4; ED: 26; DayOffset: lotIncOne),
    (SY: 1763; SM:  9; SD:  7; EY: 1763; EM: 10; ED:  6; DayOffset: lotIncOne),
    (SY: 1768; SM:  3; SD: 18; EY: 1768; EM:  4; ED: 16; DayOffset: lotIncOne),
    (SY: 1778; SM:  3; SD: 28; EY: 1778; EM:  4; ED: 26; DayOffset: lotIncOne),
    (SY: 1779; SM:  7; SD: 13; EY: 1779; EM:  8; ED: 11; DayOffset: lotIncOne),
    (SY: 1787; SM: 12; SD:  9; EY: 1788; EM:  1; ED:  7; DayOffset: lotIncOne),
    (SY: 1789; SM:  7; SD: 22; EY: 1789; EM:  8; ED: 20; DayOffset: lotIncOne),
    (SY: 1796; SM:  6; SD:  5; EY: 1796; EM:  7; ED:  4; DayOffset: lotIncOne),

    (SY: 1804; SM:  8; SD:  5; EY: 1804; EM:  9; ED:  3; DayOffset: lotIncOne),
    (SY: 1831; SM:  4; SD: 12; EY: 1831; EM:  5; ED: 11; DayOffset: lotIncOne),
    (SY: 1842; SM:  1; SD: 11; EY: 1842; EM:  2; ED:  9; DayOffset: lotIncOne),
    (SY: 1863; SM:  1; SD: 19; EY: 1863; EM:  2; ED: 17; DayOffset: lotIncOne),
    (SY: 1880; SM: 11; SD:  2; EY: 1880; EM: 12; ED:  1; DayOffset: lotDecOne),
    (SY: 1896; SM:  2; SD: 13; EY: 1896; EM:  3; ED: 13; DayOffset: lotIncOne),

    (SY: 1914; SM: 11; SD: 17; EY: 1914; EM: 12; ED: 16; DayOffset: lotIncOne),
    (SY: 1916; SM:  2; SD:  3; EY: 1916; EM:  3; ED:  3; DayOffset: lotIncOne),
    (SY: 1920; SM: 11; SD: 10; EY: 1920; EM: 12; ED:  9; DayOffset: lotIncOne),
    (SY: 1924; SM:  3; SD:  5; EY: 1924; EM:  4; ED:  3; DayOffset: lotIncOne),

    (SY: 2018; SM: 11; SD:  7; EY: 2018; EM: 12; ED:  6; DayOffset: lotDecOne),
    (SY: 2057; SM:  9; SD: 28; EY: 2057; EM: 10; ED: 27; DayOffset: lotDecOne),

    (SY: 2261; SM:  1; SD: 31; EY: 2261; EM:  3; ED:  1; DayOffset: lotIncOne),

    (SY: 2312; SM: 11; SD: 29; EY: 2312; EM: 12; ED: 28; DayOffset: lotDecOne),
    (SY: 2363; SM: 11; SD:  6; EY: 2363; EM: 12; ED:  5; DayOffset: lotDecOne),
    (SY: 2372; SM:  2; SD:  5; EY: 2372; EM:  3; ED:  5; DayOffset: lotDecOne),

    (SY: 2403; SM: 12; SD: 14; EY: 2404; EM:  1; ED: 12; DayOffset: lotDecOne),
    (SY: 2480; SM: 12; SD: 31; EY: 2481; EM:  1; ED: 29; DayOffset: lotDecOne),
    (SY: 2498; SM:  1; SD: 22; EY: 2498; EM:  2; ED: 20; DayOffset: lotDecOne),

    (SY: 2540; SM:  7; SD:  5; EY: 2540; EM:  8; ED:  3; DayOffset: lotDecOne),
    (SY: 2550; SM:  7; SD: 15; EY: 2550; EM:  8; ED: 13; DayOffset: lotDecOne),
    (SY: 2583; SM:  2; SD: 13; EY: 2583; EM:  3; ED: 14; DayOffset: lotDecOne),

    (SY: 2668; SM:  4; SD:  3; EY: 2668; EM:  5; ED:  2; DayOffset: lotDecOne),
    (SY: 2679; SM: 10; SD: 26; EY: 2679; EM: 11; ED: 24; DayOffset: lotDecOne),

    (SY: 2729; SM: 12; SD: 12; EY: 2730; EM:  1; ED: 10; DayOffset: lotDecOne)
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

// 公历年月日往前步进一天，考虑各种闰年、格里高利历删 10 天等因素
procedure StepToPreviousDay(var AYear, AMonth, ADay: Integer; AllowZeroYear: Boolean);
var
  LY: Integer;
begin
  if not AllowZeroYear then
    ValidDate(AYear, AMonth, ADay);

  if (AYear = 1582) and (AMonth = 10) and (ADay = 15) then
  begin
    ADay := 4;
    Exit;
  end
  else
  begin
    if AllowZeroYear and (AYear <= 0) then   // GetIsLeapYear 和 GetMonthDays 只接受非 0 年数
      LY := AYear - 1
    else
      LY := AYear;

    if (AMonth = 3) and (ADay = 1) then // 处理闰年的 2 月 29 日
    begin
      if GetIsLeapYear(LY) then
        ADay := 29
      else
        ADay := 28;
      Dec(AMonth);
      Exit;
    end
    else
    begin
      if ADay = 1 then // 月首，减月
      begin
        Dec(AMonth);

        if AMonth = 0 then  // 首月，退年
        begin
          AMonth := 12;

          if not AllowZeroYear and (AYear = 1) then // 公元前一年到公元元年
            AYear := -1
          else
            Dec(AYear);
        end;

        ADay := GetMonthDays(AYear, AMonth);
      end
      else
      begin
        Dec(ADay); // 非月底，减一天就行
      end;
    end;
  end;
end;

// 公历年月日往后步进一天，考虑各种闰年、格里高利历删 10 天等因素
procedure StepToNextDay(var AYear, AMonth, ADay: Integer; AllowZeroYear: Boolean);
var
  LY: Integer;
begin
  if not AllowZeroYear then
    ValidDate(AYear, AMonth, ADay);

  if (AYear = 1582) and (AMonth = 10) and (ADay = 4) then
  begin
    ADay := 15;
    Exit;
  end
  else
  begin
    if AllowZeroYear and (AYear <= 0) then   // GetIsLeapYear 和 GetMonthDays 只接受非 0 年数
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

          if not AllowZeroYear and (AYear = -1) then // 公元前一年到公元元年
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

function IsDayBetweenEqual(AYear, AMonth, ADay: Integer; StartYear, StartMonth, StartDay: Integer;
  EndYear, EndMonth, EndDay: Integer): Boolean;
begin
  Result := False;
  if (AYear < StartYear) or (AYear > EndYear) then
    Exit;

  if (AYear = StartYear) and (AMonth < StartMonth) then
    Exit;
  if (AYear = EndYear) and (AMonth > EndMonth) then
    Exit;

  if (AYear = StartYear) and (AMonth = StartMonth) and (ADay < StartDay) then
    Exit;
  if (AYear = EndYear) and (AMonth = EndMonth) and (ADay > EndDay) then
    Exit;

  Result := True;
end;

// 比较两个公历日期，1 >=< 2 分别返回 1、0、-1
function Compare2Day(Year1, Month1, Day1, Year2, Month2, Day2: Integer; CheckDay: Boolean): Integer;
begin
  if CheckDay then
  begin
    ValidDate(Year1, Month1, Day1);
    ValidDate(Year2, Month2, Day2);
  end;

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
  Result := X - Trunc(X);
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
  I, M, D, Ms, LunDay, LunDay0, WangDay: Integer;
  S, R, P, Q: Real;
  StdDays: Integer;
  PFR: PCnLunarDateFixRange;
  PFD: PCnLunarSmallMonthFix;

  function BinSearchDateFixRange(YY, MM, DD: Integer): PCnLunarDateFixRange;
  var
    Left, Right, Mid: Integer;
    CompareStart, CompareEnd: Integer;
  begin
    Result := nil;
    Left := 0;
    Right := High(CN_LUNAR_DATE_FIX);

    while Left <= Right do
    begin
      Mid := (Left + Right) div 2;

      // 检查目标日期是否在当前记录的区间内
      if IsDayBetweenEqual(YY, MM, DD,
        CN_LUNAR_DATE_FIX[Mid].SY,
        CN_LUNAR_DATE_FIX[Mid].SM,
        CN_LUNAR_DATE_FIX[Mid].SD,
        CN_LUNAR_DATE_FIX[Mid].EY,
        CN_LUNAR_DATE_FIX[Mid].EM,
        CN_LUNAR_DATE_FIX[Mid].ED) then
      begin
        Result := @CN_LUNAR_DATE_FIX[Mid];
        Exit;
      end;

      // 比较目标日期与起始日期
      CompareStart := Compare2Day(YY, MM, DD,
        CN_LUNAR_DATE_FIX[Mid].SY,
        CN_LUNAR_DATE_FIX[Mid].SM,
        CN_LUNAR_DATE_FIX[Mid].SD, False);

      // 比较目标日期与结束日期
      CompareEnd := Compare2Day(YY, MM, DD,
        CN_LUNAR_DATE_FIX[Mid].EY,
        CN_LUNAR_DATE_FIX[Mid].EM,
        CN_LUNAR_DATE_FIX[Mid].ED, False);

      if CompareStart < 0 then
      begin
        // 目标日期小于起始日期，向左搜索（因为数组是升序，较小的日期在左侧）
        Right := Mid - 1;
      end
      else if CompareEnd > 0 then
      begin
        // 目标日期大于结束日期，向右搜索（因为数组是升序，较大的日期在右侧）
        Left := Mid + 1;
      end
      else
      begin
        // 这个分支理论上不应该到达，因为如果目标日期在起始和结束之间（但不等于边界）
        // 上面的 IsDayBetweenEqual 应该已经返回 True
        // 但为了安全起见，我们向左搜索（因为起始日期小于目标日期）
        Right := Mid - 1;
      end;
    end;
  end;

  function BinSearchSmallMonthFix(YY, MM, DD: Integer): PCnLunarSmallMonthFix;
  var
    Left, Right, Mid: Integer;
    CompareResult: Integer;
  begin
    Result := nil;
    Left := Low(CN_LUNAR_YEAR_MONTH_SMALL_SUB_FIX);
    Right := High(CN_LUNAR_YEAR_MONTH_SMALL_SUB_FIX);

    while Left <= Right do
    begin
      Mid := (Left + Right) div 2;
      CompareResult := Compare2Day(YY, MM, DD,
        CN_LUNAR_YEAR_MONTH_SMALL_SUB_FIX[Mid].Y,
        CN_LUNAR_YEAR_MONTH_SMALL_SUB_FIX[Mid].M,
        CN_LUNAR_YEAR_MONTH_SMALL_SUB_FIX[Mid].D, False);

      case CompareResult of
        0:  // 找到完全匹配
          begin
            Result := @CN_LUNAR_YEAR_MONTH_SMALL_SUB_FIX[Mid];
            Exit;
          end;
        1:  // 目标日期大于中间日期（更靠近未来），由于数组是降序，向左搜索
          Right := Mid - 1;
        -1: // 目标日期小于中间日期（更靠近过去），向右搜索
          Left := Mid + 1;
      end;
    end;
  end;

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

  // 历史上的观测偏差导致的单个农历月首的单日偏差修正，包括跨年的情况
  PFR := BinSearchDateFixRange(AYear, AMonth, ADay);
  if PFR <> nil then
  begin
    if PFR^.DayOffset = lotIncOne then
    begin
      Inc(LunDay);
      D := 30;
      for M := Low(CN_LUNAR_YEAR_MONTH_SMALL_ADD_FIX) to High(CN_LUNAR_YEAR_MONTH_SMALL_ADD_FIX) do
      begin
        if (CN_LUNAR_YEAR_MONTH_SMALL_ADD_FIX[M].Y = AYear) and (CN_LUNAR_YEAR_MONTH_SMALL_ADD_FIX[M].M = AMonth)
          and (CN_LUNAR_YEAR_MONTH_SMALL_ADD_FIX[M].D = ADay) then
        begin
          D := 29;
          Break;
        end;
      end;
      if LunDay > D then // 少数情况下和 29 比较，因为内容较少，不用二分查找
        LunDay := 1;
    end
    else if PFR^.DayOffset in [lotDecOne, lotDecTwo] then
    begin
      Dec(LunDay);
      if PFR^.DayOffset = lotDecTwo then
        Dec(LunDay);

      if LunDay < 1 then
      begin
        D := 30;
        PFD := BinSearchSmallMonthFix(AYear, AMonth, ADay);
        if PFD <> nil then
          D := 29;
        LunDay := LunDay + D; // 少数情况下加 29
      end;
    end;
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

  if AYear < 237 then
    Dec(NMonth)
  else if (AYear = 237) and ((AMonth < 4) or ((AMonth = 4) and (ADay <= 11))) then
    Dec(NMonth);  // 公元 237 年农历二月直接跳到四月，早于跳变处要减一个月

  if (AYear < 24) and not ((AYear = 23) and (AMonth = 12) and (ADay = 31)) then  // 23 年 12 月 31 日也不能加 1
    Inc(NMonth);  // 公元 23 年 12 月 2 日十二月小，12 月 31 日增加十二月大但也不叫闰月

  if AYear < 9 then
    Dec(NMonth)
  else if (AYear = 9) and (AMonth = 1) and (ADay < 15) then // 公元 9 年修改月，十一月后是正月，因而正月前的月份要减一
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
