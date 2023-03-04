{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2023 CnPack 开发组                       }
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

unit CnGB18030;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：支持 GB18030 大字符集 2022 与 Unicode 的工具单元
* 单元作者：CnPack 开发组
* 备    注：GB18030 大字符集因为需兼容 GBK/GB2312，故此本质上是非等宽字符串，
*           字符长度有 ASCII 的一字节、普通汉字的二字节、生僻汉字的四字节三类
*           且均是按阅读习惯紧密排列，类似于 AnsiString
*           而 Delphi 的 WideString 和 UnicodeString 是 UTF16-LE，双字节编码有颠倒
*           比如“吃饭”两个字，
*           AnsiString 内存中是 B3D4B7B9，GB18030内码也是 B3D4 和 B7B9 符合阅读顺序
*           UnicodeString 内存中是 03546D99，但 Unicode 内码却是 5403 和 996D，有反置
*
*           GB18030 中，字符的编码值就是实际编码内容
*           UTF16 中，辅助平面内的编码值（超出二字节），和实际四字节编码方式不同
*
*           系统的 UtfEncode 函数能够正确处理四字节 UTF16-LE，注意 UTF8 转换的是
*           四字节 UTF16 字符的编码值，不是转换四字节本身，因而 UTF8-MB4 足够容纳
*
*           GB18030 的编码取值范围（十六进制）
*           注意：双字节的 AABB~CCDD 的范围不是通常意义上的增到 FF 再进位，
*             而是代表前一个字节 AA 到 CC，且后一个字节 CC 到 DD，并不包括 AAFF 这种。
*           但四字节却又不同，
*             第四字节顺序增加，但总是 30~39，没有 40，即将到 40 时便第三字节进位。一次 10 个
*             第三字节顺序增加，但总是 81~FE，没有 FF，即将到 FF 时便第二字节进位。一次 10 * 126 = 1260 个
*             第二字节顺序增加，但总是 30~39，没有 40，即将到 40 时便第一字节进位。一次 1260 * 10 = 12600 个
*             第一字节顺序增加，但总是 81~FE，没有 FF，即将到 FF 时便准备超界。    一共 12600 * 126 = 1587600 个，符合规范
*
*           单字节：00~7F
*
*           双字节：（不包括和中国无关的其他语言字符，和 Unicode 部分二字节码位杂乱对应，只能查表）
*
*                   总范围：头字节 81~FE，次字节 40~7E、80~FE          码位数 字符数
*                   8140~A07E, 8180~A0FE          3 区汉字     不连续  6080   6080   GBK 及以下
*                   A140~A77E, A180~A7A0          用户 3 区    不连续  672
*                   A1A1~A9FE                     1 区符号     不连续  846    171
*                   A840~A97E, A880~A9A0          5 区符号     不连续  192    166
*                   AA40~FE7E, AA80~FEA0          4 区汉字     不连续  8160   8160
*                   AAA1~AFFE                     用户 1 区    连续    564           E000 到 E233
*                   B0A1~F7FE                     2 区汉字     不连续  6768   6763   GB2312
*                   F8A1~FEFE                     用户 2 区    连续    658           E234 到 E4C5
*
*           四字节：（不包括和中国无关的其他语言字符）
*
*  编码范围                      码区名称                           编码位置数（容量） 有效字符数   Unicode 编码
*  四字节到 Unicode 基本平面映射
*           81308130~81318131            分隔区一                               1262                0080 到 060B，不连续，内有多处跳跃
*  81318132~81319934             维吾尔、哈萨克、柯尔克孜文一                   243    42           060C 到 06FE，开始连续
*           81319935~8132E833            分隔区二                               2049                06FF 到 0EFF  |
*  8132E834~8132FD31             藏文                                           208    193          0F00 到 0FCF  |
*           8132FD32~81339D35            分隔区三                               304                 0FD0 到 10FF  |
*  81339D36~8133B635             朝鲜文字母                                     250    69           1100 到 11F9  |
*           8133B636~8134D237            分隔区四                               1542                11FA 到 17FF  |
*  8134D238~8134E337             蒙古文（满、托忒、锡伯和阿礼嘎礼字）           170    149          1800 到 18A9  |
*           8134E338~8134F433            分隔区五                               166                 18AA 到 194F  |
*  8134F434~8134F830             德宏傣文                                       37     35           1950 到 1974  |
*           8134F831~8134F931            分隔区六                               11                  1975 到 197F  |
*  8134F932~81358437             西双版纳新傣文                                 96     83           1980 到 19DF  |
*           81358438~81358B31            分隔区七                               64                  19E0 到 1A1F  |
*  81358B32~81359935             西双版纳老傣文                                 144    127          1A20 到 1AAF，连续终止
*           81359936~81398B31            分隔区八                               4896                1AB0 到 2EFF，不连续，比如 81379735 = 24FF 和 81379736 = 254C 有跳跃
*  81398B32~8139A135             康熙部首（规范表格中结尾是 8139A035）          224    214          2F00 到 2FDF，单独连续
*           8139A136~8139A932            分隔区九                               77                  2FE0 到 3130，不连续
*  8139A933~8139B734             朝鲜文兼容字母                                 142    51           3131 到 31BE，单独连续
*           8139B735~8139EE38            分隔区十                               554                 31BF 到 33FF，不连续，比如 8139C131 = 321F 和 8139C132 = 322A 有跳跃
*  8139EE39~82358738             CJK 统一汉字扩充 A                             6530   6530         3400 到 4DB5，不连续，比如82358731 = 4DAD 和 82358732 = 4DAF 以及其他地方有五十多处跳跃
*           82358739~82358F32            分隔区十一                             74                  4DB6 到 4DFF，单独连续
*  82358F33~82359636             CJK 统一汉字                                   74     66           9FA6 到 9FEF，开始连续
*           82359637~82359832            分隔区十二                             16                  9FF0 到 9FFF  |
*  82359833~82369435             彝文                                           1223   1215         A000 到 A4C6  |
*           82369436~82369534            分隔区十三                             9                   A4C7 到 A4CF  |
*  82369535~82369A32             傈僳文                                         48     48           A4D0 到 A4FF  |
*           82369A33~8237CF34            分隔区十四                             1792                A500 到 ABFF  |
*  8237CF35~8336BE36             朝鲜文音节                                     11172  3431         AC00 到 D7A3，连续终止
*           8336BE37~8430BA31            分隔区十五                             4995                D7A4 到 FB4F，不连续，比如 8336C738 = D7FF 和 8336C739 = E76C
*  8430BA32~8430FE35             维吾尔、哈萨克、柯尔克孜文二                   684    59           FB50 到 FDFB，单独连续
*           8430FE36~84318639            分隔区十六                             64                  FDFC 到 FE6F，不连续，比如 84318537 = FE2F 和 84318538 = FE32
*  84318730~84319530             维吾尔、哈萨克、柯尔克孜文三                   141    84           FE70 到 FEFC，单独连续
*           84319531~8431A439            分隔区十七，GB18030 连续终止           159                 FEFD 到 FFFF，不连续，84319534 = FF00 比如 84319535 = FF5F
*
*  四字节到 Unicode 扩展平面映射。大范围是 90308130~E339FE39，连续线性映射到十六个平面，共 1058400 个码位
*  9034C538~9034C730             蒙古文 BIRGA，GB18030 又开始连续               13     13           11660 到 1166C，开始连续
*           9034C731~9232C635            分隔区十八，规范省略只到 9034C739      22675               1166D 到 16EFF  |
*  9232C636~9232D635             滇东北苗文                                     160    133          16F00 到 16F9F  |
*           9232D636~95328235            分隔区十九，规范省略只到 9232D639      36960               16FA0 到 1FFFF  |
*  95328236~9835F336             CJK 统一汉字扩充 B                             42711  42711        20000 到 2A6D6  |
*           9835F337~9835F737            分隔区二十                             41                  2A6D7 到 2A6FF  |
*  9835F738~98399E36             CJK 统一汉字扩充 C                             4149   4149         2A700 到 2B734  |
*           98399E37~98399F37            分隔区二十一                           11                  2B735 到 2B73F  |
*  98399F38~9839B539             CJK 统一汉字扩充 D                             222    222          2B740 到 2B81D  |
*           9839B630~9839B631            分隔区二十二                           2                   2B81E 到 2B81F  |
*  9839B632~9933FE33             CJK 统一汉字扩充 E                             5762   5762         2B820 到 2CEA1  |
*           9933FE34~99348137            分隔区二十三                           14                  2CEA2 到 2CEAF  |
*  99348138~9939F730             CJK 统一汉字扩充 F                             7473   7473         2CEB0 到 2EBE0  |
*           9939F731~9A348431            分隔区二十四                           5151                2EBE1 到 2FFFF，连续终止
*
*  FD308130~FE39FE39             用户自定义区，目前无 Unicode 映射
*
*           注意：每个四字节区的容量，均大于或等于其区内规定的有效字符数，
*              但剩余的无效字符与有效字符一样，同样有 Unicode 字符值映射。
*              并且区与区之间的分隔区域，也同样有 Unicode 字符值映射，只是没有有效字符。
*
*           所以：二、四字节 GB18010 编码与 Unicode 对应的连续区块的映射规则如下（其余八个部分还有大部分双字节只能查表）：
*
*                 AAA1~AFFE         线性对应 E000~E233    用户一区　
*                 F8A1~FEFE         线性对应 E234~E4C5    用户二区　
*                 81318132~81359935 线性对应 060C~1AAF    维吾尔、哈萨克、柯尔克孜文一头到西双版纳老傣文尾
*                 81398B32~8139A135 线性对应 2F00~2FDF    康熙部首
*                 8139A933~8139B734 线性对应 3131~31BE    朝鲜文兼容字母
*                 82358739~82358F32 线性对应 4DB6~4DFF    分隔区十一
*                 82358F33~8336BE36 线性对应 9FA6~D7A3    CJK 统一汉字头到朝鲜文音节尾
*                 8430BA32~8430FE35 线性对应 FB50~FDFB    维吾尔、哈萨克、柯尔克孜文二
*                 84318730~84319530 线性对应 FE70~FEFC    维吾尔、哈萨克、柯尔克孜文三
*                 9034C538~9A348431 线性对应 11660~2FFFF  蒙古文 BIRGA 头到分隔区二十四尾
*
*             将 GB18030 双字节编码转换为 Unicode 时，先根据双字节值确定属于上面俩区间哪个
*                然后拆成从高到低二字节，各减去 GB18010 区间开始的字节值（可能有负值）
*                两个差值分别乘以 94、1 并相加，再加上 Unicode 区间起始值即可
*
*             将 GB18030 四字节编码转换为 Unicode 时，先根据四字节值确定属于上面八区间哪个
*                然后拆成从高到低四字节，各减去 GB18010 区间开始的字节值（可能有负值），
*                四个差值分别乘以 12600、1260、10、1 并相加，再加上 Unicode 区间起始值即可
*             如果不在区间内，则只能查那两张加起来四万多项的表
*
*             单扩展位面的映射：90308130 对应 10000，到 95328235 对应 1FFFF
*                由于 90308130 的后三字节刚好是各字节的起始值，因而到 95308130 前一个，共 5 * 12600 = 63000 个码位
*                再因 95308130 的后二字节也刚好是各字节起始值，因而到 95328130 前一个，共 2 * 1260 = 2520 个码位
*                再因 95328130 的后一字节也刚好是各字节起始值，因而到 95328230 前一个，共 1 * 10 = 10 个码位
*                95328230 到 95328235，6 个码位。加起来一共 65536 个码位，正好等于一个 Unicode 扩展位面里的码位数
*
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2023.02.04
*               实现常用汉字的 Utf16 字符和 GB18030 字符的小写完整拼音获取
*           2022.11.25
*               将部分 Unicode 相关函数移至 CnWideStrings 中
*           2022.11.20
*               实现 52 个重码字的判断
*           2022.11.16
*               实现不依赖于 Windows API 的 Unicode 字符到 GB18030-2022 的转换
*           2022.11.14
*               实现不依赖于 Windows API 的 GB18030-2022 全部字符到 Unicode 的转换
*           2022.11.11
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnNative, CnWideStrings;

const
  CN_GB18030_CODEPAGE  = 54936;

type
{$IFDEF SUPPORT_ANSISTRING_CODEPAGE}
  TCnGB18030String = RawByteString;
{$ELSE}
  TCnGB18030String = AnsiString;
{$ENDIF}
  {* GB18030 编码的字符串，内部用 RawByteString 也就是 AnsiString($FFFF) 表示}

  PCnGB18130String = ^TCnGB18030String;
  {* 指向 GB18030 编码的字符串的指针}

  PCnGB18030StringPtr = PAnsiChar;
  {* GB18030 编码的字符指针，内部用 PAnsiChar 表示}

function GetCharLengthFromGB18030(GB18030Str: PCnGB18030StringPtr): Integer;
{* 计算一 GB18030 字符串的字符数}

function GetByteWidthFromGB18030(GB18030Str: PCnGB18030StringPtr): Integer;
{* 计算一 GB18030 字符串的当前字符占多少字节}

function Utf16ToGB18030(Utf16Str: PWideChar; GB18030Str: PCnGB18030StringPtr): Integer;
{* 将一 UTF16（可能混合 Unicode 扩展平面里的四字节字符）字符串转换为 GB18030 字符串
  GB18030Str 所指区域用来容纳转换的结果，如传 nil，则不进行转换
  返回值返回 GB18030Str 所需的比特长度或转换后的比特长度，不包括末尾的 #0}

function GB18030ToUtf16(GB18030Str: PCnGB18030StringPtr; Utf16Str: PWideChar): Integer;
{* 将一 GB18030 字符串转换为 UTF16（可能混合 Unicode 扩展平面里的四字节字符）字符串
  UniStr 所指区域用来容纳转换的结果，如传 nil，则不进行转换
  返回值返回 UniStr 所需的双字节字符长度或转换后的双字节字符长度，不包括末尾的宽字符 #0}

function GetGB18030FromUtf16(Utf16Str: PWideChar): TCnGB18030String;
{* 返回一 Unicode 字符串对应的 GB18030 字符串}

function GetUnicodeFromGB18030CodePoint(GBCP: TCnCodePoint): TCnCodePoint;
{* 从 GB18030 字符编码值获取其对应的 Unicode 编码值}

function GetGB18030FromUnicodeCodePoint(UCP: TCnCodePoint): TCnCodePoint;
{* 从 Unicode 字符编码值获取其对应的 GB18030 编码值}

{$IFDEF UNICODE}

function GetUtf16FromGB18030(GB18030Str: TCnGB18030String): string;
{* 返回一 GB18030 字符串对应的 Utf16 字符串}

{$ELSE}

function GetUtf16FromGB18030(GB18030Str: TCnGB18030String): WideString;
{* 返回一 GB18030 字符串对应的 Utf16 字符串}

{$ENDIF}

function GetCodePointFromGB18030Char(PtrToGB18030Chars: PCnGB18030StringPtr): TCnCodePoint;
{* 计算一个 GB18030 字符的编码值（也叫代码位置），注意 PtrToGB18030Chars 可能指向一个单、双、四字节字符}

function GetGB18030CharsFromCodePoint(CP: TCnCodePoint; PtrToChars: Pointer): Integer;
{* 计算一个 GB18030 编码值的一字节或二字节或四字节表示，如果 PtrToChars 指向的位置不为空则将转换后的内容放里头
   返回值是转换的字节数，1 或 2 或 4}

function IsUnicodeDuplicated(CP: TCnCodePoint): Boolean; overload;
{* 判断一 Unicode 编码是否是 52 个重码字之一，正式和 PUA 都算}

function IsUnicodeDuplicated(CP: TCnCodePoint; out Dup: TCnCodePoint): Boolean; overload;
{* 判断一 Unicode 编码是否是 52 个重码字之一，顺便返回重码字，正式和 PUA 都算}

function IsUnicodeDuplicated3(CP: TCnCodePoint): Boolean; overload;
{* 判断一 Unicode 编码是否是 3 个三重码字之一，俩 PUA 和一个正式}

function IsUnicodeDuplicated3(CP: TCnCodePoint; out Dup1, Dup2: TCnCodePoint): Boolean; overload;
{* 判断一 Unicode 编码是否是 3 个三重码字之一，顺便返回俩重码字，俩 PUA 和一个正式}

function IsUnicodeEqual(CP1, CP2: TCnCodePoint): Boolean;
{* 判断两个 Unicode 编码是否相等，有部分 GB18030 重码字的处理}

function CnCompareUnicodeString(US1, US2: PWideChar): Boolean;
{* 比较两个 Utf16 编码的 UnicodeString 是否相等，有 52 个重码字的处理}

function IsGBKDuplicated(CP: TCnCodePoint): Boolean;
{* 判断一个 GBK 编码是否属于 52 个重码字之一}

function IsGB18030Duplicated(CP: TCnCodePoint): Boolean;
{* 判断一个 GB18030 编码是否属于 52 个重码字之一}

function IsGB18030CodePointEqual(CP1, CP2: TCnCodePoint): Boolean;
{* 判断两个 GB18030 编码是否相等，暂无重码字的处理}

function IsGB18030Char1(CP: TCnCodePoint): Boolean;
{* 判断指定 GB18030 编码值是否合法的单字节字符}

function IsGB18030Char2(CP: TCnCodePoint): Boolean;
{* 判断指定 GB18030 编码值是否合法的双字节字符}

function IsGB18030Char4(CP: TCnCodePoint): Boolean;
{* 判断指定 GB18030 编码值是否合法的四字节字符}

function IsUnicodeInPrivateUserArea(CP: TCnCodePoint): Boolean;
{* 判断指定的 Unicode 编码值是否属于 PUA 区}

function IsGB18030InPrivateUserArea(CP: TCnCodePoint): Boolean;
{* 判断指定 GB18030 编码值是否属于 PUA 区}

function IsGB18030In2PrivateUserArea(CP: TCnCodePoint): Boolean;
{* 判断指定 GB18030 编码值是否属于双字节 PUA 区}

function IsGB18030In4PrivateUserArea(CP: TCnCodePoint): Boolean;
{* 判断指定 GB18030 编码值是否属于四字节 PUA 区}

function GetPrevGB18030CodePoint(CP: TCnCodePoint; CheckRange: Boolean = False): TCnCodePoint;
{* 获取指定 GB18030 编码值的前一个编码值，如是 0，则返回 CN_INVALID_CODEPOINT
  CheckRange 为 True 时表示会严格检查 CP 是否合法的 GB18030 编码值，是才返回前一个
  为 False 时无论该字符是否合法，均返回其前一个合法字符编码值}

function GetNextGB18030CodePoint(CP: TCnCodePoint; CheckRange: Boolean = False): TCnCodePoint;
{* 获取指定 GB18030 编码值的后一个编码值，如是最后一个，则返回 CN_INVALID_CODEPOINT
  CheckRange 为 True 时表示会严格检查 CP 是否合法的 GB18030 编码值，是才返回后一个
  为 False 时无论该字符是否合法，均返回其后一个合法字符编码值}

function GetPinYinFromUtf16Char(Utf16Char: WideChar): string;
{* 获取指定双字节 Unicode 字符的小写汉字拼音。
  只支持常用字，不支持次常用字与四字节 Unicode 字符，不处理多音字，不包括声调}

function GetPinYinFromGB18030Char(GB18030Char: WideChar): string;
{* 获取指定双字节 GB18030/GBK/GB2312 字符的小写汉字拼音。
  只支持常用字，不支持次常用字与四字节 GB18030 字符，不处理多音字，不包括声调}

implementation

uses
  CnHashMap;

type
  TCnGB18030MappingPage = packed record
  {* 记录一个连续字符的映射区间}
    GBHead: TCnCodePoint;
    GBTail: TCnCodePoint;
    UHead:  TCnCodePoint;
    UTail:  TCnCodePoint;
  end;

const
  CN_GB18030_BOM: array[0..3] of Byte = ($84, $31, $95, $33);

  // GB18030 中的 52 个重码字的编码值，源于 GBK，原始数据在双字节四区，现在扩展区
  CN_DUPLICATES_GB18030: array[0..51] of TCnCodePoint = (
    $8336C936, $8336C937, $8336CA30, $8336CA31, $8336CA32, $8336CA35, $8336CA36,
    $8336CA37, $8336CA38, $8336CA39, $8336CB30, $8336CB31, $8336CB32, $8336CB33,
    $8336CB36, $8336CB37, $8336CB39, $8336CC33, $8336CC34, $8336CC36, $8336CC37,
    $8336CC38, $8336CC39, $8336CD30, $8336CD31, $8336CD32, $8336CD33, $8336CD35,
    $8336CD36, $8336CD37, $8336CD38, $8336CD39, $8336CE30, $8336CE31, $8336CE32,
    $8336CE33, $8336CE34, $8336CE35, $8336CE36, $8336CE37, $8336CE38, $8336CE39,
    $8336CF30, $8336CF31, $8336CF32, $8336CF33, $8336CF34, $8336CF35, $8336CF36,
    $8336CF37, $8336CF38, $8336CF39
  );

  // GB18030 中的 52 个重码字的编码值，值源于 GBK，在双字节四区
  CN_DUPLICATES_GBK: array[0..51] of TCnCodePoint = (
    $FE55, $FE56, $FE5A, $FE5B, $FE5C, $FE5F, $FE60, $FE62, $FE63, $FE64, $FE65,
    $FE68, $FE69, $FE6A, $FE6F, $FE70, $FE72, $FE77, $FE78, $FE7A, $FE7B, $FE7C,
    $FE7D, $FE80, $FE81, $FE82, $FE83, $FE85, $FE86, $FE87, $FE88, $FE89, $FE8A,
    $FE8B, $FE8C, $FE8D, $FE8E, $FE8F, $FE92, $FE93, $FE94, $FE95, $FE96, $FE97,
    $FE98, $FE99, $FE9A, $FE9B, $FE9C, $FE9D, $FE9E, $FE9F
  );

  // GBK 中的 52 个重码字的 Unicode 区的正式值，都在 3400 到 4DB5 这个 CJK 扩展 A 区里
  CN_DUPLICATES_UNICODE: array[0..51] of TCnCodePoint = (
    $3473, $3447, $359E, $361A, $360E, $396E, $3918, $39CF, $39DF, $3A73, $39D0,
    $3B4E, $3C6E, $3CE0, $4056, $415F, $4337, $43B1, $43AC, $43DD, $44D6, $4661,
    $464C, $4723, $4729, $477C, $478D, $4947, $497A, $497D, $4982, $4983, $4985,
    $4986, $499F, $499B, $49B7, $49B6, $4CA3, $4C9F, $4CA0, $4CA1, $4C77, $4CA2,
    $4D13, $4D14, $4D15, $4D16, $4D17, $4D18, $4D19, $4DAE
  );

  // GBK 中的 52 个重码字在 Unicode 区原先的 PUA 值
  CN_DUPLICATES_UNICODE_PUA: array[0..51] of TCnCodePoint = (
    $E81A, $E81B, $E81F, $E820, $E821, $E824, $E825, $E827, $E828, $E829, $E82A,
    $E82D, $E82E, $E82F, $E834, $E835, $E837, $E83C, $E83D, $E83F, $E840, $E841,
    $E842, $E844, $E845, $E846, $E847, $E849, $E84A, $E84B, $E84C, $E84D, $E84E,
    $E84F, $E850, $E851, $E852, $E853, $E856, $E857, $E858, $E859, $E85A, $E85B,
    $E85C, $E85D, $E85E, $E85F, $E860, $E861, $E862, $E863
  );

  // 三个三重码字，也就是一个字有仨码！来源于 JRT0253-2022 规范
  CN_DUPLICATES_3_UNICODE: array[0..2] of array[0..2] of TCnCodePoint = (
    ($E3FE, $E579, $2B4E9),
    ($E05D, $F429, $39D1),
    ($E56B, $EAF0, $2285F)
  );

  // 双字节码转换相关
  CN_GB18030_2CHAR_PAGES: array[0..1] of TCnGB18030MappingPage = (
    (GBHead: $AAA1; GBTail: $AFFE; UHead: $E000; UTail: $E233),
    (GBHead: $F8A1; GBTail: $FEFE; UHead: $E234; UTail: $E4C5)
  );
  // 双字节三区内还有 9983~99F5 -> 6AAD~6B1F 一段有 115 个字符连续，不过忽略
  CN_GB18030_2CHAR_PAGE_COUNT = 94;

  // 四字节码转换相关。有连续完整大区（标 *** 的），也有不连续区中的连续小段区间（连续字符大于 32 个的）
  CN_GB18030_4CHAR_PAGES: array[0..65] of TCnGB18030MappingPage = (

    // 分隔区一：
    (GBHead: $81308130; GBTail: $81308435; UHead: $0080; UTail: $00A3),   // 36
    (GBHead: $81309538; GBTail: $81309F35; UHead: $016C; UTail: $01CD),   // 98
    (GBHead: $8130A331; GBTail: $8130AB37; UHead: $01FA; UTail: $0250),   // 87
    (GBHead: $8130AD33; GBTail: $8130B733; UHead: $0262; UTail: $02C6),   // 101
    (GBHead: $8130B838; GBTail: $8130CB30; UHead: $02DA; UTail: $0390),   // 183
    (GBHead: $8130CC30; GBTail: $8130D134; UHead: $03CA; UTail: $0400),   // 55
    (GBHead: $8130D330; GBTail: $81318131; UHead: $0452; UTail: $060B),   // 442

    (GBHead: $81318132; GBTail: $81359935; UHead: $060C; UTail: $1AAF),   // *** 维吾尔、哈萨克、柯尔克孜文一头到西双版纳老傣文尾

    // 分隔区八：
    (GBHead: $81359936; GBTail: $8136A531; UHead: $1AB0; UTail: $200F),   // 1376
    (GBHead: $8136A830; GBTail: $8136B331; UHead: $203C; UTail: $20AB),   // 112
    (GBHead: $8136B332; GBTail: $8136BB37; UHead: $20AD; UTail: $2102),   // 86
    (GBHead: $8136BE34; GBTail: $8136C435; UHead: $2122; UTail: $215F),   // 62
    (GBHead: $8136C734; GBTail: $8136D233; UHead: $219A; UTail: $2207),   // 110
    (GBHead: $8136D935; GBTail: $8136DD31; UHead: $2270; UTail: $2294),   // 37
    (GBHead: $8136E131; GBTail: $8136E932; UHead: $22C0; UTail: $2311),   // 82
    (GBHead: $8136E933; GBTail: $81378C35; UHead: $2313; UTail: $245F),   // 333
    (GBHead: $81378D36; GBTail: $81379735; UHead: $249C; UTail: $24FF),   // 100
    (GBHead: $8137A334; GBTail: $8137A837; UHead: $260A; UTail: $263F),   // 54
    (GBHead: $8137A839; GBTail: $8138FD38; UHead: $2643; UTail: $2E80),   // 2110
    (GBHead: $81398539; GBTail: $81398B31; UHead: $2ECB; UTail: $2EFF),   // 53

    (GBHead: $81398B32; GBTail: $8139A135; UHead: $2F00; UTail: $2FDF),   // *** 康熙部首
    (GBHead: $8139A933; GBTail: $8139B734; UHead: $3131; UTail: $31BE),   // *** 朝鲜文兼容字母

    // 分隔区十：
    (GBHead: $8139B735; GBTail: $8139C131; UHead: $31BF; UTail: $321F),   // 97
    (GBHead: $8139C139; GBTail: $8139CD31; UHead: $3232; UTail: $32A2),   // 113
    (GBHead: $8139CD32; GBTail: $8139E435; UHead: $32A4; UTail: $338D),   // 234
    (GBHead: $8139E630; GBTail: $8139E933; UHead: $33A2; UTail: $33C3),   // 34
    (GBHead: $8139EA37; GBTail: $8139EE38; UHead: $33D6; UTail: $33FF),   // 42

    // CJK 统一汉字扩充 A：
    (GBHead: $8139EE39; GBTail: $8139F539; UHead: $3400; UTail: $3446),   // 71
    (GBHead: $8139F630; GBTail: $8139FA32; UHead: $3448; UTail: $3472),   // 43
    (GBHead: $8139FA33; GBTail: $82309A30; UHead: $3474; UTail: $359D),   // 298
    (GBHead: $82309A31; GBTail: $8230A531; UHead: $359F; UTail: $360D),   // 111
    (GBHead: $8230A633; GBTail: $8230F237; UHead: $361B; UTail: $3917),   // 765
    (GBHead: $8230F238; GBTail: $8230FB32; UHead: $3919; UTail: $396D),   // 85
    (GBHead: $8230FB33; GBTail: $82318638; UHead: $396F; UTail: $39CE),   // 96
    (GBHead: $82318833; GBTail: $82319639; UHead: $39E0; UTail: $3A72),   // 147
    (GBHead: $82319730; GBTail: $8231AC37; UHead: $3A74; UTail: $3B4D),   // 218
    (GBHead: $8231AC38; GBTail: $8231C934; UHead: $3B4F; UTail: $3C6D),   // 287
    (GBHead: $8231C935; GBTail: $8231D437; UHead: $3C6F; UTail: $3CDF),   // 113
    (GBHead: $8231D438; GBTail: $8232AF32; UHead: $3CE1; UTail: $4055),   // 885
    (GBHead: $8232AF33; GBTail: $8232C936; UHead: $4057; UTail: $415E),   // 264
    (GBHead: $8232C937; GBTail: $8232F837; UHead: $4160; UTail: $4336),   // 471
    (GBHead: $8232F838; GBTail: $82338633; UHead: $4338; UTail: $43AB),   // 116
    (GBHead: $82338638; GBTail: $82338B30; UHead: $43B2; UTail: $43DC),   // 43
    (GBHead: $82338B31; GBTail: $8233A338; UHead: $43DE; UTail: $44D5),   // 248
    (GBHead: $8233A339; GBTail: $8233C931; UHead: $44D7; UTail: $464B),   // 373
    (GBHead: $8233CB32; GBTail: $8233DE34; UHead: $4662; UTail: $4722),   // 193
    (GBHead: $8233DF30; GBTail: $8233E731; UHead: $472A; UTail: $477B),   // 82
    (GBHead: $8233E838; GBTail: $82349638; UHead: $478E; UTail: $4946),   // 441
    (GBHead: $82349639; GBTail: $82349B38; UHead: $4948; UTail: $4979),   // 50
    (GBHead: $8234A131; GBTail: $8234E733; UHead: $49B8; UTail: $4C76),   // 703
    (GBHead: $8234E734; GBTail: $8234EB32; UHead: $4C78; UTail: $4C9E),   // 39
    (GBHead: $8234EB33; GBTail: $8234F633; UHead: $4CA4; UTail: $4D12),   // 111
    (GBHead: $8234F634; GBTail: $82358731; UHead: $4D1A; UTail: $4DAD),   // 148

    (GBHead: $82358739; GBTail: $82358F32; UHead: $4DB6; UTail: $4DFF),   // *** 分隔区十一
    (GBHead: $82358F33; GBTail: $8336BE36; UHead: $9FA6; UTail: $D7A3),   // *** CJK 统一汉字头到朝鲜文音节尾

    // 分隔区十五：
    (GBHead: $8336BE37; GBTail: $8336C738; UHead: $D7A4; UTail: $D7FF),   // 92
    (GBHead: $8336D030; GBTail: $84308534; UHead: $E865; UTail: $F92B),   // 4295
    (GBHead: $84308535; GBTail: $84308D30; UHead: $F92D; UTail: $F978),   // 76
    (GBHead: $84308F38; GBTail: $84309738; UHead: $F996; UTail: $F9E6),   // 81
    (GBHead: $84309C38; GBTail: $8430BA31; UHead: $FA2A; UTail: $FB4F),   // 294

    (GBHead: $8430BA32; GBTail: $8430FE35; UHead: $FB50; UTail: $FDFB),   // *** 维吾尔、哈萨克、柯尔克孜文二

    // 分隔区十六：
    (GBHead: $8430FE36; GBTail: $84318537; UHead: $FDFC; UTail: $FE2F),   // 52

    (GBHead: $84318730; GBTail: $84319530; UHead: $FE70; UTail: $FEFC),   // *** 维吾尔、哈萨克、柯尔克孜文三

    // 分隔区十七：
    (GBHead: $84319535; GBTail: $8431A233; UHead: $FF5F; UTail: $FFDF),   // 129

    (GBHead: $90308130; GBTail: $9034C537; UHead: $10000; UTail: $1165F), // *** 扩展平面起始区
    (GBHead: $9034C538; GBTail: $9A348431; UHead: $11660; UTail: $2FFFF)  // *** 蒙古文 BIRGA 头到分隔区二十四尾
  );

  CN_GB18030_4CHAR_PAGE_COUNT1 = 12600;
  CN_GB18030_4CHAR_PAGE_COUNT2 = 1260;
  CN_GB18030_4CHAR_PAGE_COUNT3 = 10;

  CN_GB18030_MAP_DEF_CAPACITY = 65536;

{$I GB18030_Unicode_2.inc}

{$I GB18030_Unicode_4.inc}

var
  F2GB18030ToUnicodeMap: TCnHashMap = nil;
  F4GB18030ToUnicodeMap: TCnHashMap = nil;
  FUnicodeToGB18030Map: TCnHashMap = nil;
  FUnicodeDuplicateMap: TCnHashMap = nil;

procedure CreateGB18030ToUnicodeMap;
var
  I: Integer;
begin
  if F2GB18030ToUnicodeMap = nil then
  begin
    F2GB18030ToUnicodeMap := TCnHashMap.Create(CN_GB18030_MAP_DEF_CAPACITY);
    for I := Low(CN_GB18030_2MAPPING) to High(CN_GB18030_2MAPPING) do
      F2GB18030ToUnicodeMap.Add(Integer(CN_GB18030_2MAPPING[I]), Integer(CN_UNICODE_2MAPPING[I]));
  end;

  if F4GB18030ToUnicodeMap = nil then
  begin
    F4GB18030ToUnicodeMap := TCnHashMap.Create(CN_GB18030_MAP_DEF_CAPACITY);
    for I := Low(CN_GB18030_4MAPPING) to High(CN_GB18030_4MAPPING) do
      F4GB18030ToUnicodeMap.Add(Integer(CN_GB18030_4MAPPING[I]), Integer(CN_UNICODE_4MAPPING[I]));
  end;
end;

procedure CreateUnicodeToGB18030Map;
var
  I: Integer;
begin
  if FUnicodeToGB18030Map = nil then
  begin
    FUnicodeToGB18030Map := TCnHashMap.Create(CN_GB18030_MAP_DEF_CAPACITY * 2);

    for I := Low(CN_UNICODE_2MAPPING) to High(CN_UNICODE_2MAPPING) do
      FUnicodeToGB18030Map.Add(Integer(CN_UNICODE_2MAPPING[I]), Integer(CN_GB18030_2MAPPING[I]));
    for I := Low(CN_UNICODE_4MAPPING) to High(CN_UNICODE_4MAPPING) do
      FUnicodeToGB18030Map.Add(Integer(CN_UNICODE_4MAPPING[I]), Integer(CN_GB18030_4MAPPING[I]));
  end;
end;

procedure CreateUnicodeDuplicateMap;
var
  I: Integer;
begin
  if FUnicodeDuplicateMap = nil then
  begin
    FUnicodeDuplicateMap := TCnHashMap.Create(256);
    for I := Low(CN_DUPLICATES_UNICODE) to High(CN_DUPLICATES_UNICODE) do
      FUnicodeDuplicateMap.Add(Integer(CN_DUPLICATES_UNICODE[I]), CN_DUPLICATES_UNICODE_PUA[I]);
    for I := Low(CN_DUPLICATES_UNICODE_PUA) to High(CN_DUPLICATES_UNICODE_PUA) do
      FUnicodeDuplicateMap.Add(Integer(CN_DUPLICATES_UNICODE_PUA[I]), CN_DUPLICATES_UNICODE[I]);
  end;
end;

procedure ExtractGB18030CodePoint(CP: TCnCodePoint; out B1, B2, B3, B4: Byte);
begin
  B1 := (CP and $FF000000) shr 24;
  B2 := (CP and $00FF0000) shr 16;
  B3 := (CP and $0000FF00) shr 8;
  B4 := CP and $000000FF;
end;

function CombineGB18030CodePoint(B1, B2, B3, B4: Byte): TCnCodePoint;
begin
  Result := (B1 shl 24) + (B2 shl 16) + (B3 shl 8) + B4;
end;

function IsGB18030CodePointEqual(CP1, CP2: TCnCodePoint): Boolean;
begin
  Result := CP1 = CP2;
end;

function IsGBKDuplicated(CP: TCnCodePoint): Boolean;
var
  I: Integer;
begin
  Result := (CP < CN_DUPLICATES_GBK[0]) or (CP > $FFFF);
  if not Result then
  begin
    for I := Low(CN_DUPLICATES_GBK) to High(CN_DUPLICATES_GBK) do
    begin
      if CP = CN_DUPLICATES_GBK[I] then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function IsGB18030Duplicated(CP: TCnCodePoint): Boolean;
var
  I: Integer;
begin
  Result := (CP < CN_DUPLICATES_GB18030[0]) or (CP > $FFFF);
  if not Result then
  begin
    for I := Low(CN_DUPLICATES_GB18030) to High(CN_DUPLICATES_GB18030) do
    begin
      if CP = CN_DUPLICATES_GB18030[I] then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function IsUnicodeEqual(CP1, CP2: TCnCodePoint): Boolean;
var
  D1, D2: TCnCodePoint;
  Is2, Is3: Boolean;
begin
  Result := CP1 = CP2;
  if not Result and (CP1 <> 0) then // 有 0 肯定不重码
  begin
    // 判断其中一个是否是二重码字
    Is2 := IsUnicodeDuplicated(CP1, D1);
    if Is2 then
      Result := D1 = CP2  // 如果 CP1 是重码字，则判断 CP2 和另一重码是否相等
    else  // 如果其中一个不是二重码字，说明还有三重码字的判断
    begin
      Is3 := IsUnicodeDuplicated3(CP1, D1, D2);
      if Is3 then // C1 是三重码字，判断 CP2 和另俩重码是否相等
        Result := (CP2 = D1) or (CP2 = D2);
    end;
  end;
end;

function CnCompareUnicodeString(US1, US2: PWideChar): Boolean;
var
  L1, L2: Integer;
  C1, C2: TCnCodePoint;
begin
  Result := False;
  if (US1 = nil) and (US2 = nil) then
  begin
    Result := True;
    Exit;
  end
  else if (US1 = nil) or (US2 = nil) then
    Exit;

  // 都有值，各自步进
  while (US1^ <> #0) and (US2^ <> #0) do
  begin
    L1 := GetByteWidthFromUtf16(US1);
    L2 := GetByteWidthFromUtf16(US2);

    if L1 <> L2 then
      Exit;

    C1 := GetCodePointFromUtf16Char(US1);
    C2 := GetCodePointFromUtf16Char(US2);

    if not IsUnicodeEqual(C1, C2) then
      Exit;

    Inc(US1, L1 shr 1);
    Inc(US2, L2 shr 1);
  end;

  Result := US1^ = US2^; // 有一个 #0 才会结束循环到这，如果都是 #0 表示后面没了，相等
end;

function IsUnicodeDuplicated(CP: TCnCodePoint): Boolean;
var
  C: TCnCodePoint;
begin
  Result := IsUnicodeDuplicated(CP, C);
end;

function IsUnicodeDuplicated(CP: TCnCodePoint; out Dup: TCnCodePoint): Boolean;
var
  C: Integer;
begin
  Result := FUnicodeDuplicateMap.Find(CP, C);
  if Result then
    Dup := TCnCodePoint(C);
end;

function IsUnicodeDuplicated3(CP: TCnCodePoint): Boolean;
var
  I, J: Integer;
begin
  Result := True;
  for I := Low(CN_DUPLICATES_3_UNICODE) to High(CN_DUPLICATES_3_UNICODE) do
  begin
    // 遍历每个仨值组
    for J := Low(CN_DUPLICATES_3_UNICODE[I]) to High(CN_DUPLICATES_3_UNICODE[I]) do
    begin
      if CP = CN_DUPLICATES_3_UNICODE[I][J] then
        Exit;
    end;
  end;
  Result := False;
end;

function IsUnicodeDuplicated3(CP: TCnCodePoint; out Dup1, Dup2: TCnCodePoint): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := Low(CN_DUPLICATES_3_UNICODE) to High(CN_DUPLICATES_3_UNICODE) do
  begin
    // 遍历每个仨值组
    if CP = CN_DUPLICATES_3_UNICODE[I][0] then
    begin
      Dup1 := CN_DUPLICATES_3_UNICODE[I][1];
      Dup2 := CN_DUPLICATES_3_UNICODE[I][2];
      Exit;
    end
    else if CP = CN_DUPLICATES_3_UNICODE[I][1] then
    begin
      Dup1 := CN_DUPLICATES_3_UNICODE[I][0];
      Dup2 := CN_DUPLICATES_3_UNICODE[I][2];
      Exit;
    end
    else if CP = CN_DUPLICATES_3_UNICODE[I][2] then
    begin
      Dup1 := CN_DUPLICATES_3_UNICODE[I][0];
      Dup2 := CN_DUPLICATES_3_UNICODE[I][1];
      Exit;
    end;
  end;
  Result := False;
end;

function IsGB18030Char1(CP: TCnCodePoint): Boolean;
begin
  Result := CP in [$00..$7F];
end;

function IsGB18030Char2(CP: TCnCodePoint): Boolean;
var
  B1, B2, B3, B4: Byte;
begin
  ExtractGB18030CodePoint(CP, B1, B2, B3, B4);
  Result := (B1 = 0) and (B2 = 0) and ((B3 >= $81) and (B3 <= $FE)) and
    (((B4 >= $40) and (B4 <= $7E)) or ((B4 >= $80) and (B4 <= $FE)));
end;

function IsGB18030Char4(CP: TCnCodePoint): Boolean;
var
  B1, B2, B3, B4: Byte;
begin
  ExtractGB18030CodePoint(CP, B1, B2, B3, B4);
  Result := ((B1 >= $81) and (B1 <= $FE)) and ((B2 >= $30) and (B2 <= $39))
    and ((B3 >= $81) and (B3 <= $FE)) and ((B4 >= $30) and (B4 <= $39));
end;

function IsUnicodeInPrivateUserArea(CP: TCnCodePoint): Boolean;
begin
  Result := ((CP >= $E000) and (CP <= $F8FF)) or
    ((CP >= $F0000) and (CP <= $FFFFD)) or
    ((CP >= $100000) and (CP <= $10FFFD));
end;

function IsGB18030InPrivateUserArea(CP: TCnCodePoint): Boolean;
begin
  Result := IsGB18030In2PrivateUserArea(CP) or IsGB18030In4PrivateUserArea(CP);
end;

function IsGB18030In2PrivateUserArea(CP: TCnCodePoint): Boolean;
var
  B1, B2, B3, B4: Byte;
begin
  Result := IsGB18030Char2(CP);
  if Result then
  begin
    ExtractGB18030CodePoint(CP, B1, B2, B3, B4);
    Result := ((B3 >= $AA) and (B3 <= $AF) and (B4 >= $A1) and (B4 <= $FE))  // 双字节用户一区
      or ((B3 >= $F8) and (B3 <= $FE) and (B4 >= $A1) and (B4 <= $FE))       // 双字节用户二区
      or (((B3 >= $A1) and (B3 <= $A7)) and ((B4 >= $40) and (B4 <= $7E) or (B4 >= $80) and (B4 <= $A0))); // 双字节用户三区
  end;
end;

function IsGB18030In4PrivateUserArea(CP: TCnCodePoint): Boolean;
var
  B1, B2, B3, B4: Byte;
begin
  Result := IsGB18030Char4(CP);
  if Result then
  begin
    ExtractGB18030CodePoint(CP, B1, B2, B3, B4);
    Result := (B1 >= $FD) and (B1 <= $FE); // 其余判断均在 IsGB18030Char4 中
  end;
end;

function GetPrevGB18030CodePoint(CP: TCnCodePoint; CheckRange: Boolean): TCnCodePoint;
var
  B1, B2, B3, B4: Byte;
begin
  Result := CN_INVALID_CODEPOINT;
  if CP = 0 then
    Exit;

  if CheckRange and (CP in [$80..$FF]) and not IsGB18030Char2(CP) and not IsGB18030Char4(CP) then
    Exit;

  if CP <= $7F then
    Result := CP - 1
  else if CP <= $8140 then    // 单字节和双字节间的空白，前一个取单字节字符最大值
    Result := $7F
  else if CP <= $FEFE then    // 8141~FEFE 头缺一个
  begin
    // 二字节区域
    ExtractGB18030CodePoint(CP, B1, B2, B3, B4);
    if B4 <= $40 then
    begin
      B4 := $FE;
      Dec(B3);
    end
    else
      Dec(B4);

    if B4 = $7F then
      Dec(B4);

    Result := CombineGB18030CodePoint(0, 0, B3, B4);
  end
  else if CP <= $81308130 then // 二字节和四字节间的空白，前一个取双字节字符最大值
    Result := $FEFE
  else if CP <= $FE39FE39 then // 四字节头缺一个
  begin
    // 四字节区域
    ExtractGB18030CodePoint(CP, B1, B2, B3, B4);
    if B4 <= $30 then
    begin
      B4 := $39;
      Dec(B3);
      if B3 < $81 then
      begin
        B3 := $FE;
        Dec(B2);
        if B2 < $30 then
        begin
          B2 := $39;
          Dec(B1);
        end;
        if B1 < $81 then // 出错退出
          Exit;
      end;
    end
    else
      Dec(B4);

    Result := CombineGB18030CodePoint(B1, B2, B3, B4);
  end
  else
    Result := $FE39FE39;
end;

function GetNextGB18030CodePoint(CP: TCnCodePoint; CheckRange: Boolean): TCnCodePoint;
var
  B1, B2, B3, B4: Byte;
begin
  Result := CN_INVALID_CODEPOINT;
  if CP >= $FE39FE39 then
    Exit;

  if CheckRange and (CP in [$80..$FF]) and not IsGB18030Char2(CP) and not IsGB18030Char4(CP) then
    Exit;

  if CP < $7F then
    Result := CP + 1
  else if CP < $8140 then    // 单字节和双字节间的空白，后一个取双字节字符最小值
    Result := $8140
  else if CP < $FEFE then    // 8140~FEFD 尾缺一个
  begin
    // 二字节区域
    ExtractGB18030CodePoint(CP, B1, B2, B3, B4);
    if B4 = $FE then
    begin
      B4 := $40;
      Inc(B3);
    end
    else
    begin
      Inc(B4);
      if B4 = $7F then
        Inc(B4);
    end;
    Result := CombineGB18030CodePoint(0, 0, B3, B4);
  end
  else if CP < $81308130 then // 二字节和四字节间的空白，后一个取四字节字符最小值
    Result := $81308130
  else if CP < $FE39FE39 then // 四字节尾缺一个
  begin
    // 四字节区域
    ExtractGB18030CodePoint(CP, B1, B2, B3, B4);

    if B4 >= $39 then
    begin
      B4 := $30;
      Inc(B3);
      if B3 > $FE then
      begin
        B3 := $81;
        Inc(B2);
        if B2 > $39 then
        begin
          B2 := $30;
          Inc(B1);
        end;
        if B1 > $FE then // 出错退出
          Exit;
      end;
    end
    else
      Inc(B4);

    Result := CombineGB18030CodePoint(B1, B2, B3, B4);
  end;
end;

function GetCharLengthFromGB18030(GB18030Str: PCnGB18030StringPtr): Integer;
var
  L: Integer;
begin
  Result := 0;
  while GB18030Str^ <> #0 do
  begin
    L := GetByteWidthFromGB18030(GB18030Str);
    Inc(GB18030Str, L);
    Inc(Result);
  end;
end;

function GetByteWidthFromGB18030(GB18030Str: PCnGB18030StringPtr): Integer;
var
  B1, B2, B3, B4: Byte;
begin
  Result := 1;
  B1 := Byte(GB18030Str^);
  if B1 <= $7F then
    Exit;

  Inc(GB18030Str);
  B2 := Byte(GB18030Str^);

  if (B1 >= $81) and (B1 <= $FE) then
  begin
    if ((B2 >= $40) and (B2 <= $7E)) or
      ((B2 >= $80) and (B2 <= $FE)) then
      Result := 2
    else if (B2 >= $30) and (B2 <= $39) then
    begin
      Inc(GB18030Str);
      B3 := Byte(GB18030Str^);
      Inc(GB18030Str);
      B4 := Byte(GB18030Str^);

      if ((B3 >= $81) and (B3 <= $FE)) or
      ((B4 >= $30) and (B4 <= $39)) then
        Result := 4;
    end;
  end;
end;

function Utf16ToGB18030(Utf16Str: PWideChar; GB18030Str: PCnGB18030StringPtr): Integer;
var
  W: Integer;
  GBCP, UCP: TCnCodePoint;
begin
  Result := 0;
  if Utf16Str = nil then
    Exit;

  while Utf16Str^ <> #0 do
  begin
    W := GetByteWidthFromUtf16(Utf16Str);
    UCP := GetCodePointFromUtf16Char(Utf16Str);
    GBCP := GetGB18030FromUnicodeCodePoint(UCP);
    Inc(Utf16Str, W shr 1);

    if GBCP = CN_INVALID_CODEPOINT then // 非法 GB18030 字符，用一个问号代替
    begin
      if GB18030Str <> nil then
      begin
        GB18030Str^ := CN_ALTERNATIVE_CHAR;
        Inc(GB18030Str);
      end;

      Inc(Result);
    end
    else // 合法的 GB18030 字符
    begin
      W := GetGB18030CharsFromCodePoint(GBCP, GB18030Str);
      if GB18030Str <> nil then
        Inc(GB18030Str, W);

      Inc(Result, W);
    end;
  end;
end;

function GB18030ToUtf16(GB18030Str: PCnGB18030StringPtr; Utf16Str: PWideChar): Integer;
var
  W: Integer;
  GBCP, UCP: TCnCodePoint;
begin
  Result := 0;
  if GB18030Str = nil then
    Exit;

  while GB18030Str^ <> #0 do
  begin
    W := GetByteWidthFromGB18030(GB18030Str);
    GBCP := GetCodePointFromGB18030Char(GB18030Str);
    UCP := GetUnicodeFromGB18030CodePoint(GBCP);
    Inc(GB18030Str, W);

    if UCP = CN_INVALID_CODEPOINT then // 非法 Unicode 字符，用一个问号代替
    begin
      if Utf16Str <> nil then
      begin
        Utf16Str^ := CN_ALTERNATIVE_CHAR;
        Inc(Utf16Str);
      end;

      Inc(Result);
    end
    else // 合法的 Unicode 字符
    begin
      W := GetUtf16CharFromCodePoint(UCP, Utf16Str);
      if Utf16Str <> nil then
        Inc(Utf16Str, W);

      Inc(Result, W);
    end;
  end;
end;

function GetUnicodeFromGB18030CodePoint(GBCP: TCnCodePoint): TCnCodePoint;
var
  I, GBBase, UBase: TCnCodePoint;
  A1, A2, B1, B2, B3, B4, C1, C2, C3, C4: Byte;
  D1, D2, D3, D4: Integer;
begin
  Result := CN_INVALID_CODEPOINT;

  if GBCP < $80 then
    Result := GBCP
  else if GBCP < $FFFF then
  begin
    // 查双字节表
    GBBase := 0;
    UBase := 0;

    B1 := (GBCP and $0000FF00) shr 8;
    B2 := GBCP and $000000FF;

    for I := Low(CN_GB18030_2CHAR_PAGES) to High(CN_GB18030_2CHAR_PAGES) do
    begin
      A1 := (CN_GB18030_2CHAR_PAGES[I].GBHead and $0000FF00) shr 8;
      A2 := CN_GB18030_2CHAR_PAGES[I].GBHead and $000000FF;
      C1 := (CN_GB18030_2CHAR_PAGES[I].GBTail and $0000FF00) shr 8;
      C2 := CN_GB18030_2CHAR_PAGES[I].GBTail and $000000FF;

      // 双字节区间有交叉，不能直接比较大小以判断位置，得拆分比较
      if (B1 >= A1) and (B1 <= C1) and (B2 >= A2) and (B2 <= C2) then
      begin
        GBBase := CN_GB18030_2CHAR_PAGES[I].GBHead;
        UBase := CN_GB18030_2CHAR_PAGES[I].UHead;
        Break;
      end;
    end;

    if GBBase > 0 then
    begin
      B1 := (GBBase and $0000FF00) shr 8;
      B2 := GBBase and $000000FF;

      C1 := (GBCP and $0000FF00) shr 8;
      C2 := GBCP and $000000FF;

      D1 := C1 - B1;   // 需要用 Integer，因为可能有负值
      D2 := C2 - B2;

      Result := TCnCodePoint(Int64(D1 * CN_GB18030_2CHAR_PAGE_COUNT + D2) + Int64(UBase));
    end
    else
    begin
      // 查六个二字节组合成的表
      UBase := F2GB18030ToUnicodeMap.Find(Integer(GBCP));
      if UBase > 0 then
        Result := UBase;
    end;
  end
  else
  begin
    // 四字节
    GBBase := 0;
    UBase := 0;

    for I := Low(CN_GB18030_4CHAR_PAGES) to High(CN_GB18030_4CHAR_PAGES) do
    begin
      if (GBCP >= CN_GB18030_4CHAR_PAGES[I].GBHead) and (GBCP <= CN_GB18030_4CHAR_PAGES[I].GBTail) then
      begin
        GBBase := CN_GB18030_4CHAR_PAGES[I].GBHead;
        UBase := CN_GB18030_4CHAR_PAGES[I].UHead;
        Break;
      end;
    end;

    if GBBase > 0 then
    begin
      ExtractGB18030CodePoint(GBBase, B1, B2, B3, B4);
      ExtractGB18030CodePoint(GBCP, C1, C2, C3, C4);

      D1 := C1 - B1;   // 需要用 Integer，因为可能有负值
      D2 := C2 - B2;
      D3 := C3 - B3;
      D4 := C4 - B4;

      Result := TCnCodePoint(Int64(D1 * CN_GB18030_4CHAR_PAGE_COUNT1 + D2 * CN_GB18030_4CHAR_PAGE_COUNT2
        + D3 * CN_GB18030_4CHAR_PAGE_COUNT3 + D4) + Int64(UBase));
    end
    else
    begin
      // 查八个四字节表
      UBase := F4GB18030ToUnicodeMap.Find(Integer(GBCP));
      if UBase > 0 then
        Result := UBase;
    end;
  end;
end;

function GetGB18030FromUnicodeCodePoint(UCP: TCnCodePoint): TCnCodePoint;
var
  I, GBBase, UBase: TCnCodePoint;
  A1, A2, B1, B2, B3, B4, C1, C2, C3, C4: Byte;
  D1, D2, D3, D4: Cardinal;
begin
  Result := CN_INVALID_CODEPOINT;

  if UCP < $80 then
    Result := UCP
  else // 不分 Unicode 范围，先查两个区间，再查 Map
  begin
    // 查双字节区间表
    UBase := 0;
    GBBase := 0;

    for I := Low(CN_GB18030_2CHAR_PAGES) to High(CN_GB18030_2CHAR_PAGES) do
    begin
      if (UCP >= CN_GB18030_2CHAR_PAGES[I].UHead) and (UCP <= CN_GB18030_2CHAR_PAGES[I].UTail) then
      begin
        UBase := CN_GB18030_2CHAR_PAGES[I].UHead;
        GBBase := CN_GB18030_2CHAR_PAGES[I].GBHead;
        Break;
      end;
    end;

    if UBase > 0 then
    begin
      // 如何双字节逆计算？
      UCP := UCP - UBase;

      A1 := UCP div 94;
      A2 := UCP mod 94;

      B1 := (GBBase and $0000FF00) shr 8;
      B2 := GBBase and $000000FF;

      D1 := A1 + B1;
      D2 := A2 + B2;
      if D2 > $FE then
      begin
        Dec(D2, 94);
        Inc(D1);
      end;

      Result := (D1 shl 8) + D2;
    end
    else // 查四字节区间表
    begin
      GBBase := 0;
      UBase := 0;

      for I := Low(CN_GB18030_4CHAR_PAGES) to High(CN_GB18030_4CHAR_PAGES) do
      begin
        if (UCP >= CN_GB18030_4CHAR_PAGES[I].UHead) and (UCP <= CN_GB18030_4CHAR_PAGES[I].UTail) then
        begin
          UBase := CN_GB18030_4CHAR_PAGES[I].UHead;
          GBBase := CN_GB18030_4CHAR_PAGES[I].GBHead;
          Break;
        end;
      end;

      if GBBase > 0 then
      begin
        // 四字节逆计算
        UCP := UCP - UBase;
        C1 := UCP div 12600;
        C2 := (UCP - 12600 * C1) div 1260;
        C3 := (UCP - 12600 * C1- 1260 * C2) div 10;
        C4 := UCP - 12600 * C1- 1260 * C2 - 10 * C3;

        ExtractGB18030CodePoint(GBBase, B1, B2, B3, B4);

        A1 := UnsignedAddWithLimitRadix(C4, B4, $0, D4, $30, $39);  // 最低位相加，进位供后面使用
        A1 := UnsignedAddWithLimitRadix(C3, B3, A1, D3, $81, $FE);  // 次低位相加，进位供后面使用
        A1 := UnsignedAddWithLimitRadix(C2, B2, A1, D2, $30, $39);
        A1 := UnsignedAddWithLimitRadix(C1, B1, A1, D1, $81, $FE);  // 最高位相加，不应有进位

        if A1 = 0 then
          Result := CombineGB18030CodePoint(D1, D2, D3, D4);     // 拼出结果
      end;
    end;

    if Result = CN_INVALID_CODEPOINT then
    begin
      // 查组合成的大表
      GBBase := FUnicodeToGB18030Map.Find(Integer(UCP));
      if GBBase > 0 then
        Result := GBBase;
    end;
  end;
end;

function GetGB18030FromUtf16(Utf16Str: PWideChar): TCnGB18030String;
var
  L: Integer;
begin
  L := Utf16ToGB18030(Utf16Str, nil);
  if L > 0 then
  begin
    SetLength(Result, L);
    Utf16ToGB18030(Utf16Str, @Result[1]);
  end;
end;

{$IFDEF UNICODE}

function GetUtf16FromGB18030(GB18030Str: TCnGB18030String): string;
var
  L: Integer;
begin
  L := GB18030ToUtf16(PCnGB18030StringPtr(GB18030Str), nil);
  if L > 0 then
  begin
    SetLength(Result, L);
    GB18030ToUtf16(PCnGB18030StringPtr(GB18030Str), @Result[1]);
  end;
end;

{$ELSE}

function GetUtf16FromGB18030(GB18030Str: TCnGB18030String): WideString;
var
  L: Integer;
begin
  L := GB18030ToUtf16(PCnGB18030StringPtr(GB18030Str), nil);
  if L > 0 then
  begin
    SetLength(Result, L);
    GB18030ToUtf16(PCnGB18030StringPtr(GB18030Str), @Result[1]);
  end;
end;

{$ENDIF}

function GetCodePointFromGB18030Char(PtrToGB18030Chars: PCnGB18030StringPtr): TCnCodePoint;
var
  C1, C2, C3, C4: Byte;
begin
  Result := CN_INVALID_CODEPOINT;

  C1 := Byte(PtrToGB18030Chars^);
  if C1 < $80 then
    Result := C1                                // 单字节
  else if (C1 >= $81) and (C1 <= $FE) then
  begin
    Inc(PtrToGB18030Chars);
    C2 := Byte(PtrToGB18030Chars^);
    if ((C2 >= $40) and (C2 <= $7E)) or ((C2 >= $90) and (C2 <= $FE)) then
      Result := C1 shl 8 + C2                   // 双字节
    else if (C2 >= $30) and (C2 <= $39) then    // 四字节
    begin
      Inc(PtrToGB18030Chars);
      C3 := Byte(PtrToGB18030Chars^);
      Inc(PtrToGB18030Chars);
      C4 := Byte(PtrToGB18030Chars^);

      // 再判断三字节的 81 到 FE 以及四字节的 30 到 39
      if (C3 >= $81) and (C3 <= $FE) and (C4 >= $30) and (C4 <= $39) then
        Result := CombineGB18030CodePoint(C1, C2, C3, C4);
    end;
  end;
end;

function GetGB18030CharsFromCodePoint(CP: TCnCodePoint; PtrToChars: Pointer): Integer;
var
  P: PByte;
  C1, C2, C3, C4: Byte;
begin
  Result := 0;
  P := PByte(PtrToChars);
  if CP < $80 then
  begin
    if P <> nil then
      P^ := Byte(CP);
    Result := 1;
  end
  else
  begin
    ExtractGB18030CodePoint(CP, C1, C2, C3, C4);

    if (C1 = 0) and (C2 = 0) and ((C3 >= $81) and (C3 <= $FE)) and
      (((C4 >= $40) and (C4 <= $7E)) or ((C4 >= $80) and (C4 <= $FE))) then
    begin
      // 是两字节字符
      if P <> nil then
      begin
        P^ := C3;
        Inc(P);
        P^ := C4;
      end;
      Result := 2;
    end
    else if ((C1 >= $81) and (C1 <= $FE)) and ((C2 >= $30) and (C2 <= $39))
      and ((C3 >= $81) and (C3 <= $FE)) and ((C4 >= $30) and (C4 <= $39)) then
    begin
      // 是四字节字符
      if P <> nil then
      begin
        P^ := C1;
        Inc(P);
        P^ := C2;
        Inc(P);
        P^ := C3;
        Inc(P);
        P^ := C4;
      end;
      Result := 4;
    end;
  end;
end;

function GetPinYinFromGB18030CodePoint(GBCP: TCnCodePoint): string;
begin
  // 根据常用字的拼音分布，用码点分区间值进行判断
  case GBCP of
    45217..45218: Result := 'a';
    45219..45231: Result := 'ai';
    45232..45240: Result := 'an';
    45241..45243: Result := 'ang';
    45244..45252: Result := 'ao';
    45253..45270: Result := 'ba';
    45271..45278: Result := 'bai';
    45279..45293: Result := 'ban';
    45294..45305: Result := 'bang';
    45306..45484: Result := 'bao';
    45485..45499: Result := 'bei';
    45500..45503: Result := 'ben';
    45504..45509: Result := 'beng';
    45510..45533: Result := 'bi';
    45534..45545: Result := 'bian';
    45546..45549: Result := 'biao';
    45550..45553: Result := 'bie';
    45554..45559: Result := 'bin';
    45560..45730: Result := 'bing';
    45731..45751: Result := 'bo';
    45752..45760: Result := 'bu';
    45761..45761: Result := 'ca';
    45762..45772: Result := 'cai';
    45773..45779: Result := 'can';
    45780..45784: Result := 'cang';
    45785..45789: Result := 'cao';
    45790..45794: Result := 'ce';
    45795..45796: Result := 'ceng';
    45797..45807: Result := 'cha';
    45808..45810: Result := 'chai';
    45811..45820: Result := 'chan';
    45821..45995: Result := 'chang';
    45996..46004: Result := 'chao';
    46005..46010: Result := 'che';
    46011..46020: Result := 'chen';
    46021..46035: Result := 'cheng';
    46036..46051: Result := 'chi';
    46052..46056: Result := 'chong';
    46057..46068: Result := 'chou';
    46069..46246: Result := 'chu';
    46247..46247: Result := 'chuai';
    46248..46254: Result := 'chuan';
    46255..46260: Result := 'chuang';
    46261..46265: Result := 'chui';
    46266..46272: Result := 'chun';
    46273..46274: Result := 'chuo';
    46275..46286: Result := 'ci';
    46287..46292: Result := 'cong';
    46293..46293: Result := 'cou';
    46294..46297: Result := 'cu';
    46298..46300: Result := 'cuan';
    46301..46308: Result := 'cui';
    46309..46311: Result := 'cun';
    46312..46317: Result := 'cuo';
    46318..46323: Result := 'da';
    46324..46497: Result := 'dai';
    46498..46512: Result := 'dan';
    46513..46517: Result := 'dang';
    46518..46529: Result := 'dao';
    46530..46532: Result := 'de';
    46533..46539: Result := 'deng';
    46540..46558: Result := 'di';
    46559..46574: Result := 'dian';
    46575..46583: Result := 'diao';
    46584..46752: Result := 'die';
    46753..46761: Result := 'ding';
    46762..46762: Result := 'diu';
    46763..47009: Result := 'dong';
    47010..47017: Result := 'fa';
    47018..47034: Result := 'fan';
    47035..47045: Result := 'fang';
    47046..47057: Result := 'fei';
    47058..47072: Result := 'fen';
    47073..47087: Result := 'feng';
    47088..47088: Result := 'fo';
    47089..47089: Result := 'fou';
    47090..47296: Result := 'fu';
    47297..47298: Result := 'ga';
    47299..47304: Result := 'gai';
    47305..47315: Result := 'gan';
    47316..47324: Result := 'gang';
    47325..47334: Result := 'gao';
    47335..47351: Result := 'ge';
    47352..47352: Result := 'gei';
    47353..47354: Result := 'gen';
    47355..47523: Result := 'geng';
    47524..47538: Result := 'gong';
    47539..47547: Result := 'gou';
    47548..47565: Result := 'gu';
    47566..47571: Result := 'gua';
    47572..47574: Result := 'guai';
    47575..47585: Result := 'guan';
    47586..47588: Result := 'guang';
    47589..47604: Result := 'gui';
    47605..47607: Result := 'gun';
    47608..47613: Result := 'guo';
    47614..47776: Result := 'ha';
    47777..47783: Result := 'hai';
    47784..47802: Result := 'han';
    47803..47805: Result := 'hang';
    47806..47814: Result := 'hao';
    47815..47832: Result := 'he';
    47833..47834: Result := 'hei';
    47835..47838: Result := 'hen';
    47839..47843: Result := 'heng';
    47844..47852: Result := 'hong';
    47853..47859: Result := 'hou';
    47860..48039: Result := 'hu';
    48040..48048: Result := 'hua';
    48049..48053: Result := 'huai';
    48054..48067: Result := 'huan';
    48068..48081: Result := 'huang';
    48082..48102: Result := 'hui';
    48103..48108: Result := 'hun';
    48109..48118: Result := 'huo';
    48119..48333: Result := 'ji';
    48334..48350: Result := 'jia';
    48351..48552: Result := 'jian';
    48553..48565: Result := 'jiang';
    48566..48593: Result := 'jiao';
    48594..48620: Result := 'jie';
    48621..48802: Result := 'jin';
    48803..48827: Result := 'jing';
    48828..48829: Result := 'jiong';
    48830..48846: Result := 'jiu';
    48847..48871: Result := 'ju';
    48872..48878: Result := 'juan';
    48879..48888: Result := 'jue';
    48889..49061: Result := 'jun';
    49062..49065: Result := 'ka';
    49066..49070: Result := 'kai';
    49071..49076: Result := 'kan';
    49077..49083: Result := 'kang';
    49084..49087: Result := 'kao';
    49088..49102: Result := 'ke';
    49103..49106: Result := 'ken';
    49107..49108: Result := 'keng';
    49109..49112: Result := 'kong';
    49113..49116: Result := 'kou';
    49117..49123: Result := 'ku';
    49124..49128: Result := 'kua';
    49129..49132: Result := 'kuai';
    49133..49134: Result := 'kuan';
    49135..49142: Result := 'kuang';
    49143..49315: Result := 'kui';
    49316..49319: Result := 'kun';
    49320..49323: Result := 'kuo';
    49324..49330: Result := 'la';
    49331..49333: Result := 'lai';
    49334..49348: Result := 'lan';
    49349..49355: Result := 'lang';
    49356..49364: Result := 'lao';
    49365..49366: Result := 'le';
    49367..49377: Result := 'lei';
    49378..49380: Result := 'leng';
    49381..49576: Result := 'li';
    49577..49577: Result := 'lia';
    49578..49591: Result := 'lian';
    49592..49602: Result := 'liang';
    49603..49615: Result := 'liao';
    49616..49620: Result := 'lie';
    49621..49632: Result := 'lin';
    49633..49646: Result := 'ling';
    49647..49657: Result := 'liu';
    49658..49828: Result := 'long';
    49829..49834: Result := 'lou';
    49835..49854: Result := 'lu';
    49855..49868: Result := 'lv';
    49869..49874: Result := 'luan';
    49875..49876: Result := 'lue';
    49877..49883: Result := 'lun';
    49884..49895: Result := 'luo';
    49896..49904: Result := 'ma';
    49905..49910: Result := 'mai';
    49911..50081: Result := 'man';
    50082..50087: Result := 'mang';
    50088..50099: Result := 'mao';
    50100..50100: Result := 'me';
    50101..50116: Result := 'mei';
    50117..50119: Result := 'men';
    50120..50127: Result := 'meng';
    50128..50141: Result := 'mi';
    50142..50150: Result := 'mian';
    50151..50158: Result := 'miao';
    50159..50160: Result := 'mie';
    50161..50166: Result := 'min';
    50167..50172: Result := 'ming';
    50173..50173: Result := 'miu';
    50174..50352: Result := 'mo';
    50353..50355: Result := 'mou';
    50356..50370: Result := 'mu';
    50371..50377: Result := 'na';
    50378..50382: Result := 'nai';
    50383..50385: Result := 'nan';
    50386..50386: Result := 'nang';
    50387..50391: Result := 'nao';
    50392..50392: Result := 'ne';
    50393..50394: Result := 'nei';
    50395..50395: Result := 'nen';
    50396..50396: Result := 'neng';
    50397..50407: Result := 'ni';
    50408..50414: Result := 'nian';
    50415..50416: Result := 'niang';
    50417..50418: Result := 'niao';
    50419..50425: Result := 'nie';
    50426..50426: Result := 'nin';
    50427..50594: Result := 'ning';
    50595..50598: Result := 'niu';
    50599..50602: Result := 'nong';
    50603..50605: Result := 'nu';
    50606..50606: Result := 'nv';
    50607..50607: Result := 'nuan';
    50608..50609: Result := 'nue';
    50610..50613: Result := 'nuo';
    50614..50614: Result := 'o';
    50615..50621: Result := 'ou';
    50622..50627: Result := 'pa';
    50628..50633: Result := 'pai';
    50634..50641: Result := 'pan';
    50642..50646: Result := 'pang';
    50647..50653: Result := 'pao';
    50654..50662: Result := 'pei';
    50663..50664: Result := 'pen';
    50665..50678: Result := 'peng';
    50679..50857: Result := 'pi';
    50858..50861: Result := 'pian';
    50862..50865: Result := 'piao';
    50866..50867: Result := 'pie';
    50868..50872: Result := 'pin';
    50873..50881: Result := 'ping';
    50882..50890: Result := 'po';
    50891..50905: Result := 'pu';
    50906..50941: Result := 'qi';
    50942..51106: Result := 'qia';
    51107..51128: Result := 'qian';
    51129..51136: Result := 'qiang';
    51137..51151: Result := 'qiao';
    51152..51156: Result := 'qie';
    51157..51167: Result := 'qin';
    51168..51180: Result := 'qing';
    51181..51182: Result := 'qiong';
    51183..51190: Result := 'qiu';
    51191..51365: Result := 'qu';
    51366..51376: Result := 'quan';
    51377..51384: Result := 'que';
    51385..51386: Result := 'qun';
    51387..51390: Result := 'ran';
    51391..51395: Result := 'rang';
    51396..51398: Result := 'rao';
    51399..51400: Result := 're';
    51401..51410: Result := 'ren';
    51411..51412: Result := 'reng';
    51413..51413: Result := 'ri';
    51414..51423: Result := 'rong';
    51424..51426: Result := 'rou';
    51427..51436: Result := 'ru';
    51437..51438: Result := 'ruan';
    51439..51441: Result := 'rui';
    51442..51443: Result := 'run';
    51444..51445: Result := 'ruo';
    51446..51448: Result := 'sa';
    51449..51452: Result := 'sai';
    51453..51618: Result := 'san';
    51619..51621: Result := 'sang';
    51622..51625: Result := 'sao';
    51626..51628: Result := 'se';
    51629..51629: Result := 'sen';
    51630..51630: Result := 'seng';
    51631..51639: Result := 'sha';
    51640..51641: Result := 'shai';
    51642..51657: Result := 'shan';
    51658..51665: Result := 'shang';
    51666..51676: Result := 'shao';
    51677..51688: Result := 'she';
    51689..51704: Result := 'shen';
    51705..51877: Result := 'sheng';
    51878..51924: Result := 'shi';
    51925..51934: Result := 'shou';
    51935..52129: Result := 'shu';
    52130..52131: Result := 'shua';
    52132..52135: Result := 'shuai';
    52136..52137: Result := 'shuan';
    52138..52140: Result := 'shuang';
    52141..52144: Result := 'shui';
    52145..52148: Result := 'shun';
    52149..52152: Result := 'shuo';
    52153..52168: Result := 'si';
    52169..52176: Result := 'song';
    52177..52179: Result := 'sou';
    52180..52192: Result := 'su';
    52193..52195: Result := 'suan';
    52196..52206: Result := 'sui';
    52207..52209: Result := 'sun';
    52210..52217: Result := 'suo';
    52218..52388: Result := 'ta';
    52389..52397: Result := 'tai';
    52398..52415: Result := 'tan';
    52416..52428: Result := 'tang';
    52429..52439: Result := 'tao';
    52440..52440: Result := 'te';
    52441..52444: Result := 'teng';
    52445..52459: Result := 'ti';
    52460..52467: Result := 'tian';
    52468..52472: Result := 'tiao';
    52473..52475: Result := 'tie';
    52476..52647: Result := 'ting';
    52648..52660: Result := 'ng';
    52661..52664: Result := 'u';
    52665..52675: Result := 'tu';
    52676..52677: Result := 'tuan';
    52678..52683: Result := 'tui';
    52684..52686: Result := 'tun';
    52687..52697: Result := 'tuo';
    52698..52704: Result := 'wa';
    52705..52706: Result := 'wai';
    52707..52723: Result := 'wan';
    52724..52733: Result := 'wang';
    52734..52928: Result := 'wei';
    52929..52938: Result := 'wen';
    52939..52941: Result := 'weng';
    52942..52950: Result := 'wo';
    52951..52979: Result := 'wu';
    52980..53176: Result := 'xi';
    53177..53189: Result := 'xia';
    53190..53215: Result := 'xian';
    53216..53235: Result := 'xiang';
    53236..53415: Result := 'xiao';
    53416..53436: Result := 'xie';
    53437..53446: Result := 'xin';
    53447..53461: Result := 'xing';
    53462..53468: Result := 'xiong';
    53469..53477: Result := 'xiu';
    53478..53496: Result := 'xu';
    53497..53668: Result := 'xuan';
    53669..53674: Result := 'xue';
    53675..53688: Result := 'xun';
    53689..53704: Result := 'ya';
    53705..53737: Result := 'yan';
    53738..53754: Result := 'yang';
    53755..53931: Result := 'yao';
    53932..53946: Result := 'ye';
    53947..53999: Result := 'yi';
    54000..54177: Result := 'yin';
    54178..54195: Result := 'ying';
    54196..54196: Result := 'yo';
    54197..54211: Result := 'yong';
    54212..54232: Result := 'you';
    54233..54438: Result := 'yu';
    54439..54458: Result := 'yuan';
    54459..54468: Result := 'yue';
    54469..54480: Result := 'yun';
    54481..54483: Result := 'za';
    54484..54490: Result := 'zai';
    54491..54494: Result := 'zan';
    54495..54497: Result := 'zang';
    54498..54511: Result := 'zao';
    54512..54515: Result := 'ze';
    54516..54516: Result := 'zei';
    54517..54517: Result := 'zen';
    54518..54521: Result := 'zeng';
    54522..54697: Result := 'zha';
    54698..54703: Result := 'zhai';
    54704..54720: Result := 'zhan';
    54721..54735: Result := 'zhang';
    54736..54745: Result := 'zhao';
    54746..54755: Result := 'zhe';
    54756..54771: Result := 'zhen';
    54772..54948: Result := 'zheng';
    54949..54991: Result := 'zhi';
    54992..55002: Result := 'zhong';
    55003..55016: Result := 'zhou';
    55017..55204: Result := 'zhu';
    55205..55206: Result := 'zhua';
    55207..55207: Result := 'zhuai';
    55208..55213: Result := 'zhuan';
    55214..55220: Result := 'zhuang';
    55221..55226: Result := 'zhui';
    55227..55228: Result := 'zhun';
    55229..55239: Result := 'zhuo';
    55240..55254: Result := 'zi';
    55255..55261: Result := 'zong';
    55262..55265: Result := 'zou';
    55266..55273: Result := 'zu';
    55274..55275: Result := 'zuan';
    55276..55279: Result := 'zui';
    55280..55281: Result := 'zun';
    55282..55282: Result := 'zuo';
  else
    Result := '';
  end;
end;

function GetPinYinFromUtf16Char(Utf16Char: WideChar): string;
var
  UCP, GBCP: TCnCodePoint;
begin
  UCP := GetCodePointFromUtf16Char(@Utf16Char);    // Utf16 字符转换为 Unicode 码点
  GBCP := GetGB18030FromUnicodeCodePoint(UCP);     // Unicode 码点转换为 GB18030 码点

  Result := GetPinYinFromGB18030CodePoint(GBCP);
end;

function GetPinYinFromGB18030Char(GB18030Char: WideChar): string;
var
  GBS: array[0..1] of WideChar;
  GBCP: TCnCodePoint;
begin
  GBS[0] := GB18030Char;
  GBS[1] := #0;

  GBCP := GetCodePointFromGB18030Char(PCnGB18030StringPtr(@GBS[0]));
  Result := GetPinYinFromGB18030CodePoint(GBCP);
end;

initialization
  CreateGB18030ToUnicodeMap;
  CreateUnicodeToGB18030Map;
  CreateUnicodeDuplicateMap;

finalization
  F2GB18030ToUnicodeMap.Free;
  F4GB18030ToUnicodeMap.Free;
  FUnicodeToGB18030Map.Free;
  FUnicodeDuplicateMap.Free;

end.

