{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2022 CnPack 开发组                       }
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
* 修改记录：2022.11.20
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

// {$DEFINE UTF16_BE}

// Delphi 默认 UTF16-LE，如果要处理 UTF16-BE 字符串，需要定义 UTF16_BE

uses
  SysUtils, Classes, CnNative;

const
  CN_GB18030_CODEPAGE  = 54936;
  CN_INVALID_CODEPOINT = $FFFFFFFF;
  CN_ALTERNATIVE_CHAR  = '?';

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

  TCnCodePoint = type Cardinal;
  {* 字符码值，或者叫码点，不等于表达的编码方式}

  TCn2CharRec = packed record
  {* 双字节字符结构}
    P1: AnsiChar;
    P2: AnsiChar;
  end;
  PCn2CharRec = ^TCn2CharRec;

  TCn4CharRec = packed record
  {* 四字节字符结构}
    P1: AnsiChar;
    P2: AnsiChar;
    P3: AnsiChar;
    P4: AnsiChar;
  end;
  PCn4CharRec = ^TCn4CharRec;

function GetCharLengthFromUtf8(Utf8Str: PAnsiChar): Integer;
{* 计算一 UTF8（可能是 UTF8-MB4）字符串的字符数}

function GetCharLengthFromUtf16(Utf16Str: PWideChar): Integer;
{* 计算一 UTF16（可能混合 Unicode 扩展平面里的四字节字符）字符串的字符数}

function GetCharLengthFromGB18030(GB18030Str: PCnGB18030StringPtr): Integer;
{* 计算一 GB18030 字符串的字符数}

function GetByteWidthFromUtf8(Utf8Str: PAnsiChar): Integer;
{* 计算一 UTF8（可能是 UTF8-MB4）字符串的当前字符占多少字节}

function GetByteWidthFromUtf16(Utf16Str: PWideChar): Integer;
{* 计算一 UTF16（可能混合 Unicode 扩展平面里的四字节字符）字符串的当前字符占多少字节}

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

function GetCodePointFromUtf16Char(Utf16Str: PWideChar): TCnCodePoint;
{* 计算一个 Utf16 字符的编码值（也叫代码位置），注意 Utf16Str 可能指向一个双字节字符，也可能指向一个四字节字符}

function GetCodePointFromUtf164Char(PtrTo4Char: Pointer): TCnCodePoint;
{* 计算一个四字节 Utf16 字符的编码值（也叫代码位置）}

function GetUtf16CharFromCodePoint(CP: TCnCodePoint; PtrToChars: Pointer): Integer;
{* 计算一个 Unicode 编码值的二字节或四字节表示，如果 PtrToChars 指向的位置不为空，
  则将结果放在 PtrToChars 所指的二字节或四字节区域
  调用者在 CP 超过 $FFFF 时须保证 PtrToChars 所指的区域至少四字节，反之二字节即可
  返回 1 或 2，分别表示处理的是二字节或四字节}

function GetCodePointFromGB18030Char(PtrToGB18030Chars: PCnGB18030StringPtr): TCnCodePoint;
{* 计算一个 GB18030 字符的编码值（也叫代码位置），注意 PtrToGB18030Chars 可能指向一个单、双、四字节字符}

function GetGB18030CharsFromCodePoint(CP: TCnCodePoint; PtrToChars: Pointer): Integer;
{* 计算一个 GB18030 编码值的一字节或二字节或四字节表示，如果 PtrToChars 指向的位置不为空则将转换后的内容放里头
   返回值是转换的字节数，1 或 2 或 4}

function IsUnicodeDuplicated(CP: TCnCodePoint): Boolean; overload;
{* 判断一 Unicode 编码是否是 52 个重码字之一，正式和 PUA 都算}

function IsUnicodeDuplicated(CP: TCnCodePoint; out Dup: TCnCodePoint): Boolean; overload;
{* 判断一 Unicode 编码是否是 52 个重码字之一，顺便返回重码字，正式和 PUA 都算}

function IsUnicodeEqual(CP1, CP2: TCnCodePoint): Boolean;
{* 判断两个 Unicode 编码是否相等，有部分 GB18030 重码字的处理}

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

function GetUtf16HighByte(Rec: PCn2CharRec): Byte; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* 得到一个 UTF 16 双字节字符的高位字节值}

function GetUtf16LowByte(Rec: PCn2CharRec): Byte; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* 得到一个 UTF 16 双字节字符的低位字节值}

procedure SetUtf16HighByte(B: Byte; Rec: PCn2CharRec); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* 设置一个 UTF 16 双字节字符的高位字节值}

procedure SetUtf16LowByte(B: Byte; Rec: PCn2CharRec); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* 设置一个 UTF 16 双字节字符的低位字节值}

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
  CN_UTF16_4CHAR_PREFIX1_LOW  = $D8;
  CN_UTF16_4CHAR_PREFIX1_HIGH = $DC;
  CN_UTF16_4CHAR_PREFIX2_LOW  = $DC;
  CN_UTF16_4CHAR_PREFIX2_HIGH = $E0;

  CN_UTF16_4CHAR_HIGH_MASK    = $3;
  CN_UTF16_4CHAR_SPLIT_MASK   = $3FF;

  CN_UTF16_EXT_BASE           = $10000;

  CN_GB18030_BOM: array[0..3] of Byte = ($84, $31, $95, $33);

  // GB18030 中的 52 个重码字的编码值，值源于 GBK，在双字节四区
  CN_DUPLICATES_GB18030: array[0..51] of TCnCodePoint = (
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
begin
  Result := CP1 = CP2;
  if not Result and (CP1 <> 0) then // 有 0 肯定不重码
  begin
    // 判断重码字
    CP1 := TCnCodePoint(FUnicodeDuplicateMap.Find(CP1));
    Result := CP1 = CP2;
  end;
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

function GetUtf16HighByte(Rec: PCn2CharRec): Byte;
begin
{$IFDEF UTF16_BE}
  Result := Byte(Rec^.P1);
{$ELSE}
  Result := Byte(Rec^.P2); // UTF16-LE 的高低位会置换
{$ENDIF}
end;

function GetUtf16LowByte(Rec: PCn2CharRec): Byte;
begin
{$IFDEF UTF16_BE}
  Result := Byte(Rec^.P2);
{$ELSE}
  Result := Byte(Rec^.P1); // UTF16-LE 的高低位会置换
{$ENDIF}
end;

procedure SetUtf16HighByte(B: Byte; Rec: PCn2CharRec);
begin
{$IFDEF UTF16_BE}
  Rec^.P1 := AnsiChar(B);
{$ELSE}
  Rec^.P2 := AnsiChar(B); // UTF16-LE 的高低位会置换
{$ENDIF}
end;

procedure SetUtf16LowByte(B: Byte; Rec: PCn2CharRec);
begin
{$IFDEF UTF16_BE}
  Rec^.P2 := AnsiChar(B);
{$ELSE}
  Rec^.P1 := AnsiChar(B); // UTF16-LE 的高低位会置换
{$ENDIF}
end;

function GetCharLengthFromUtf8(Utf8Str: PAnsiChar): Integer;
var
  L: Integer;
begin
  Result := 0;
  while Utf8Str^ <> #0 do
  begin
    L := GetByteWidthFromUtf8(Utf8Str);
    Inc(Utf8Str, L);
    Inc(Result);
  end;
end;

function GetCharLengthFromUtf16(Utf16Str: PWideChar): Integer;
var
  L: Integer;
begin
  Result := 0;
  while Utf16Str^ <> #0 do
  begin
    L := GetByteWidthFromUtf16(Utf16Str);
    Utf16Str := PWideChar(TCnNativeInt(Utf16Str) + L);
    Inc(Result);
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

function GetByteWidthFromUtf8(Utf8Str: PAnsiChar): Integer;
var
  B: Byte;
begin
  B := Byte(Utf8Str^);
  if B >= $FC then        // 6 个 1，1 个 0，先不考虑七或八 1 的情况
    Result := 6
  else if B >= $F8 then   // 5 个 1，1 个 0
    Result := 5
  else if B >= $F0 then   // 4 个 1，1 个 0
    Result := 4
  else if B >= $E0 then   // 3 个 1，1 个 0
    Result := 3
  else if B >= $B0 then   // 2 个 1，1 个 0
    Result := 2
  else                    // 其他
    Result := 1;
end;

function GetByteWidthFromUtf16(Utf16Str: PWideChar): Integer;
var
  P: PCn2CharRec;
  B1, B2: Byte;
begin
  Result := 2;

  P := PCn2CharRec(Utf16Str);
  B1 := GetUtf16HighByte(P);

  if (B1 >= CN_UTF16_4CHAR_PREFIX1_LOW) and (B1 < CN_UTF16_4CHAR_PREFIX1_HIGH) then
  begin
    // 如果两个单字节字符，其值分别在 $D800 到 $DBFF 之间
    Inc(P);
    B2 := GetUtf16HighByte(P);

    // 那么紧跟在后面的两个单字节字符应该在 $DC00 到 $DFFF 之间，
    if (B2 >= CN_UTF16_4CHAR_PREFIX2_LOW) and (B2 < CN_UTF16_4CHAR_PREFIX2_HIGH) then
      Result := 4;

    // 这四个字节组成一个四字节 Unicode 字符，但并非该值的编码值
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

      Result := D1 * CN_GB18030_2CHAR_PAGE_COUNT + D2 + UBase;
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

      Result := D1 * CN_GB18030_4CHAR_PAGE_COUNT1 + D2 * CN_GB18030_4CHAR_PAGE_COUNT2
        + D3 * CN_GB18030_4CHAR_PAGE_COUNT3 + D4 + UBase;
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

function GetCodePointFromUtf16Char(Utf16Str: PWideChar): TCnCodePoint;
var
  R: Word;
  C2: PCn2CharRec;
begin
  if GetByteWidthFromUtf16(Utf16Str) = 4 then // 四字节字符
    Result := GetCodePointFromUtf164Char(PAnsiChar(Utf16Str))
  else  // 普通双字节字符
  begin
    C2 := PCn2CharRec(Utf16Str);
    R := Byte(C2^.P1) shl 8 + Byte(C2^.P2);       // 双字节字符，值本身就是编码值

{$IFDEF UTF16_BE}
    Result := TCnCodePoint(R);
{$ELSE}
    Result := TCnCodePoint(UInt16ToBigEndian(R)); // UTF16-LE 要交换值
{$ENDIF}
  end;
end;

function GetCodePointFromUtf164Char(PtrTo4Char: Pointer): TCnCodePoint;
var
  TH, TL: Word;
  C2: PCn2CharRec;
begin
  C2 := PCn2CharRec(PtrTo4Char);

  // 第一个字节，去掉高位的 110110；第二个字节留着，共 2 + 8 = 10 位
  TH := (GetUtf16HighByte(C2) and CN_UTF16_4CHAR_HIGH_MASK) shl 8 + GetUtf16LowByte(C2);
  Inc(C2);

  // 第三个字节，去掉高位的 110111，第四个字节留着，共 2 + 8 = 10 位
  TL := (GetUtf16HighByte(C2) and CN_UTF16_4CHAR_HIGH_MASK) shl 8 + GetUtf16LowByte(C2);

  // 高 10 位拼低 10 位
  Result := TH shl 10 + TL + CN_UTF16_EXT_BASE;
  // 码点减去 $10000 后的值，前 10 位映射到 $D800 到 $DBFF 之间，后 10 位映射到 $DC00 到 $DFFF 之间
end;

function GetUtf16CharFromCodePoint(CP: TCnCodePoint; PtrToChars: Pointer): Integer;
var
  C2: PCn2CharRec;
  L, H: Byte;
  LW, HW: Word;
begin
  if CP >= CN_UTF16_EXT_BASE then
  begin
    if PtrToChars <> nil then
    begin
      CP := CP - CN_UTF16_EXT_BASE;
      // 拆出高 10 位放前两字节，拆出低 10 位放后两字节

      LW := CP and CN_UTF16_4CHAR_SPLIT_MASK;          // 低 10 位，放三、四字节
      HW := (CP shr 10) and CN_UTF16_4CHAR_SPLIT_MASK; // 高 10 位，放一、二字节

      L := HW and $FF;
      H := (HW shr 8) and CN_UTF16_4CHAR_HIGH_MASK;
      H := H or CN_UTF16_4CHAR_PREFIX1_LOW;              // 1101 1000
      C2 := PCn2CharRec(PtrToChars);

      SetUtf16LowByte(L, C2);
      SetUtf16HighByte(H, C2);

      L := LW and $FF;
      H := (LW shr 8) and CN_UTF16_4CHAR_HIGH_MASK;
      H := H or CN_UTF16_4CHAR_PREFIX1_HIGH;              // 1101 1100
      Inc(C2);

      SetUtf16LowByte(L, C2);
      SetUtf16HighByte(H, C2);
    end;
    Result := 2;
  end
  else
  begin
    if PtrToChars <> nil then
    begin
      C2 := PCn2CharRec(PtrToChars);
      SetUtf16LowByte(Byte(CP and $00FF), C2);
      SetUtf16HighByte(Byte(CP shr 8), C2);
    end;
    Result := 1;
  end;
end;

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

