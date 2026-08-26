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

unit CnBerUtils;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：ASN.1 格式的 BER/DER 编解码实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元实现了 ASN.1 格式的 BER/DER 编解码，提供了 TCnBerReader 类可将
*           内容解析为树状数据结构，提供了 TCnBerWriter 类实现数据组装并输出。
* 开发平台：WinXP + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2023.12.13 V1.6
*               增加对 00 00 不固定长度节点的解析支持，待进一步测试
*           2022.04.26 V1.5
*               修改 LongWord 与 Integer 地址转换以支持 MacOS64
*           2022.04.15 V1.4
*               加入一 AsCommonInteger 方法允许自动根据数据长度 1、2、4 获取整型值。
*           2020.03.28 V1.3
*               允许外部给节点设置 TypeMask 以应对 ECC 的私钥父节点的情况。
*           2019.04.19 V1.2
*               支持 Win32/Win64/MacOS，支持 VCL 与 FMX 下的 TreeView 交互。
*           2018.05.27 V1.1
*               将 Parser 改为 Reader 并实现 Writer
*           2018.05.24 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

// 如需要启用和 TreeView 交互的功能，需在工程选项中定义 ENABLE_UIINTERACT

// 用 ENABLE_FMX 来控制 FMX 环境下是否支持 FMX，默认不支持，以避免编译出来的东西体积太大
{$IFNDEF ENABLE_FMX}
  {$UNDEF SUPPORT_FMX}
{$ENDIF}

uses
  SysUtils, Classes, TypInfo, CnNative, CnBigNumber, CnTree
  {$IFDEF DEBUG} {$IFDEF ENABLE_UIINTERACT}
    {$IFDEF MSWINDOWS}, ComCtrls  {$ENDIF} // 如果 Windows 下编译错误找不到该单元，请在编译选项里加 Vcl 前缀
    {$IFDEF SUPPORT_FMX}, FMX.TreeView {$ENDIF}
  {$ENDIF} {$ENDIF};
  // If ComCtrls not found, please add 'Vcl' to 'Unit Scope Names' in Project Options.

const
  CN_BER_TAG_TYPE_MASK                      = $C0;
  {* Ber Tag 的类型掩码，代表一字节内的最高两位：00 为 Universal，01 为 Application，10 为 Context-Specific，11 为 Private}

  CN_BER_TAG_STRUCT_MASK                    = $20;
  {* Ber Tag 的结构掩码}

  CN_BER_TAG_VALUE_MASK                     = $1F;
  {* Ber Tag 的值掩码}

  CN_BER_LENLEN_MASK                        = $80;
  {* Ber Tag 的长度的长度掩码}

  CN_BER_LENGTH_MASK                        = $7F;
  {* Ber Tag 的长度掩码}

  CN_BER_MAX_DEPTH                          = 200;
  {* Ber 解析的最多轮数避免堆栈溢出}

  CN_BER_TAG_RESERVED                       = $00;
  {* Ber Tag 类型 0，保留}

  CN_BER_TAG_BOOLEAN                        = $01;
  {* 布尔类型的 Ber Tag}

  CN_BER_TAG_INTEGER                        = $02;
  {* 整数类型的 Ber Tag}

  CN_BER_TAG_BIT_STRING                     = $03;
  {* 位串类型的 Ber Tag}

  CN_BER_TAG_OCTET_STRING                   = $04;
  {* 字符串类型的 Ber Tag}

  CN_BER_TAG_NULL                           = $05;
  {* Null 类型的 Ber Tag}

  CN_BER_TAG_OBJECT_IDENTIFIER              = $06;
  {* 对象标识符类型的 Ber Tag}

  CN_BER_TAG_OBJECT_DESCRIPION              = $07;
  {* 对象描述类型的 Ber Tag}

  CN_BER_TAG_EXTERNAL                       = $08;
  {* 外部类型的 Ber Tag}

  CN_BER_TAG_REAL                           = $09;
  {* 浮点数类型的 Ber Tag}

  CN_BER_TAG_ENUMERATED                     = $0A;
  {* 枚举类型的 Ber Tag}

  CN_BER_TAG_EMBEDDED_PDV                   = $0B;
  {* 嵌入类型的 Ber Tag}

  CN_BER_TAG_UFT8STRING                     = $0C;
  {* UTF-8 字符串类型的 Ber Tag}

  CN_BER_TAG_RELATIVE_OID                   = $0D;
  {* 关联对象描述类型的 Ber Tag}

  CN_BER_TAG_SEQUENCE                       = $10;
  {* 序列类型的 Ber Tag}

  CN_BER_TAG_SET                            = $11;
  {* 集合类型的 Ber Tag}

  CN_BER_TAG_NUMERICSTRING                  = $12;
  {* 数字字符串类型的 Ber Tag}

  CN_BER_TAG_PRINTABLESTRING                = $13;
  {* 可打印字符串类型的 Ber Tag}

  CN_BER_TAG_TELETEXSTRING                  = $14;
  {* T61String 字符串类型的 Ber Tag}

  CN_BER_TAG_VIDEOTEXSTRING                 = $15;
  {* 视频字符串类型的 Ber Tag}

  CN_BER_TAG_IA5STRING                      = $16;
  {* IA5 字符串类型的 Ber Tag}

  CN_BER_TAG_UTCTIME                        = $17;
  {* 协调世界时的日期时间类型的 Ber Tag}

  CN_BER_TAG_GENERALIZEDTIME                = $18;
  {* 通用日期时间类型的 Ber Tag}

  CN_BER_TAG_GRAPHICSTRING                  = $19;
  {* 图像字符串类型的 Ber Tag}

  CN_BER_TAG_VISIBLESTRING                  = $1A;
  {* 可见字符串类型的 Ber Tag}

  CN_BER_TAG_GENERALSTRING                  = $1B;
  {* 普通字符串类型的 Ber Tag}

  CN_BER_TAG_UNIVERSALSTRING                = $1C;
  {* 通用字符串类型的 Ber Tag}

  CN_BER_TAG_CHARACTER_STRING               = $1D;
  {* 字符化字符串类型的 Ber Tag}

  CN_BER_TAG_BMPSTRING                      = $1E;
  {* 位图字符串类型的 Ber Tag}

type
  ECnBerException = class(Exception);
  {* BER 相关异常}

  ECnBerDepthOverflowError = class(ECnBerException);
  {* 嵌套深度超限异常，属于致命错误，内层解析失败时不得吞掉该类异常}

  TCnBerTagRange = CN_BER_TAG_BOOLEAN..CN_BER_TAG_BMPSTRING;
  {* BER 的 Tag 范围}

  TCnBerTagSet = set of TCnBerTagRange;
  {* BER 的 Tag 集合}

  TCnBerTag = (cbtReserved_0, cbtBoolean, cbtInteger, cbtBit_String,
    cbtOctet_String, cbtNull, cbtObject_Identifier, cbtObject_Descripion,
    cbtExternal, cbtReal, cbtEnumerated, cbtEmbedded_Pdv, cbtUft8String,
    cbtRelative_Oid, cbtReserved_0E, cbtReserved_0F, cbtSequence, cbtSet,
    cbtNumericString, cbtPrintableString, cbtTeletexString, cbtVideotexString,
    cbtIa5String, cbtUtcTime, cbtGeneralizedTime, cbtGraphicString,
    cbtVisibleString, cbtGeneralString, cbtUniversalString, cbtCharacter_String,
    cbtBmpstring);
  {* BER 的 Tag 枚举定义}

  TCnBerTags = set of TCnBerTag;
  {* BER 的 Tag 枚举集合}

  TCnBerReadNode = class(TCnLeaf)
  {* 描述一解析出来的 ASN.1 节点}
  private
    FOriginData: PByte;
    FBerLength: Integer;
    FBerOffset: Integer;
    FBerTag: Integer;
    FBerTagClass: Integer;
    FBerDataLength: Integer;
    FBerDataOffset: Integer;
    function GetItems(AIndex: Integer): TCnBerReadNode;
    procedure SetItems(AIndex: Integer; const Value: TCnBerReadNode);

    function InternalAsInteger(ByteSize: Integer): Integer;
    function InternalAsString(TagSet: TCnBerTagSet): AnsiString;
    function GetBerDataAddress: Pointer;
    function GetBerAddress: Pointer;
  public
    procedure CopyDataTo(DestBuf: Pointer);
    {* 将数据复制至缓冲区，缓冲区尺寸至少需要 BerDataLength 字节大小。

       参数：
         DestBuf: Pointer                 - 容纳待输出内容的数据块地址

       返回值：（无）
    }

    procedure CopyHeadTo(DestBuf: Pointer);
    {* 将节点头部也就是 TL 内容复制至缓冲区，缓冲区尺寸至少需要 BerLength - BerDataLength 字节大小。

       参数：
         DestBuf: Pointer                 - 容纳待输出内容的数据块地址

       返回值：（无）
    }

    procedure CopyTLVTo(DestBuf: Pointer);
    {* 将节点全部内容复制至缓冲区，缓冲区尺寸至少需要 BerLength 字节大小。

       参数：
         DestBuf: Pointer                 - 容纳待输出内容的数据块地址

       返回值：（无）
    }

    function AsBoolean: Boolean;
    {* 返回布尔值。

       参数：
         （无）

       返回值：Boolean                    - 返回布尔值
    }
    function AsShortInt: ShortInt;
    {* 返回 8 位有符号整数。

       参数：
         （无）

       返回值：ShortInt                   - 返回 8 位有符号整数
    }

    function AsByte: Byte;
    {* 返回 8 位无符号整数。

       参数：
         （无）

       返回值：Byte                       - 返回 8 位无符号整数
    }

    function AsSmallInt: SmallInt;
    {* 返回 8 位有符号整数。

       参数：
         （无）

       返回值：SmallInt                   - 返回 16 位有符号整数
    }

    function AsWord: Word;
    {* 返回 16 位无符号整数。

       参数：
         （无）

       返回值：Word                       - 返回 16 位无符号整数
    }

    function AsInteger: Integer;
    {* 返回 32 位有符号整数。

       参数：
         （无）

       返回值：Integer                    - 返回 32 位有符号整数
    }

    function AsCardinal: Cardinal;
    {* 返回 32 位无符号整数。

       参数：
         （无）

       返回值：Cardinal                   - 返回 32 位无符号整数
    }

    function AsInt64: Int64;
    {* 返回 64 位有符号整数。

       参数：
         （无）

       返回值：Int64                      - 返回 64 位有符号整数
    }

    // 注意以上整型返回的方法，调用时应当与 BerDataLength 对应，否则倒序时会出错

    function AsCommonInteger: Integer;
    {* 该方法按 BerDataLength 的实际值返回整型并按实际倒序，BerDataLength 超出 Integer 时出错。

       参数：
         （无）

       返回值：Integer                    - 返回整型值
    }

    procedure AsBigNumber(OutNum: TCnBigNumber);
    {* 按尺寸返回整型大数。

       参数：
         OutNum: TCnBigNumber             - 容纳返回整数的大数

       返回值：（无）
    }

    function AsRawString: string;
    {* 直接返回字符串，不限制类型。

       参数：
         （无）

       返回值：string                     - 返回字符串
    }

    function AsAnsiString: AnsiString;
    {* 返回单字节字符串。

       参数：
         （无）

       返回值：AnsiString                 - 返回单字节字符串
    }

    function AsString: string;
    {* 返回字符串，限制为几种字符串类型。

       参数：
         （无）

       返回值：string                     - 返回字符串
    }

    function AsPrintableString: string;
    {* 返回可打印字符串。

       参数：
         （无）

       返回值：string                     - 返回可打印字符串
    }

    function AsIA5String: string;
    {* 返回纯 ASCII 码的字符串。

       参数：
         （无）

       返回值：string                     - 返回纯 ASCII 码字符串
    }

    function AsDateTime: TDateTime;
    {* 严格解析并返回 UTCTime 或 GeneralizedTime 对应的 UTC 日期时间。
       输入必须被完整消费并显式包含 Z 或 +hhmm/-hhmm 时区；不使用本地化日期格式。

       参数：
         （无）

       返回值：TDateTime                  - 返回日期时间
    }

    function IsNull: Boolean;
    {* 返回是否为 Null。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 Null
    }

    function IsString: Boolean;
    {* 返回是否为字符串。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为字符串
    }
    function IsInteger: Boolean;
    {* 返回是否为整数。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为整数
    }

    function IsDateTime: Boolean;
    {* 返回是否为日期时间。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为日期时间
    }

    function GetNextSibling: TCnBerReadNode;
    {* 返回下一个同级节点。

       参数：
         （无）

       返回值：TCnBerReadNode             - 返回下一个同级节点
    }

    function GetPrevSibling: TCnBerReadNode;
    {* 返回上一个同级节点。

       参数：
         （无）

       返回值：TCnBerReadNode             - 返回上一个同级节点
    }

    property Items[AIndex: Integer]: TCnBerReadNode read GetItems write SetItems; default;
    {* 条目索引}

    property BerOffset: Integer read FBerOffset write FBerOffset;
    {* 该节点对应的 ASN.1 内容编码在整体中的偏移}
    property BerAddress: Pointer read GetBerAddress;
    {* 整个节点的内容起始地址，也就是 FOriginData + FBerOffset}
    property BerLength: Integer read FBerLength write FBerLength;
    {* 整个节点的内容长度}

    property BerTag: Integer read FBerTag write FBerTag;
    {* 节点类型，也就是 Tag}
    property BerDataLength: Integer read FBerDataLength write FBerDataLength;
    {* 节点数据长度}
    property BerDataOffset: Integer read FBerDataOffset write FBerDataOffset;
    {* 该节点对应的数据内容在整体中的偏移}
    property BerDataAddress: Pointer read GetBerDataAddress;
    {* 该节点对应的数据的起始地址，等于 FOriginData + FBerDataOffset}
  end;

  TCnBerReader = class(TObject)
  {* 读取并解析 BER 编码数据块的解析器类}
  private
    FBerTree: TCnTree;
    FData: PByte;
    FDataByteLen: Cardinal;
    FParseInnerString: Boolean;
    FCurrentIsBitString: Boolean;
{$IFDEF DEBUG}
{$IFDEF ENABLE_UIINTERACT}
  {$IFDEF MSWINDOWS}
    function GetOnSaveNode: TCnTreeNodeEvent;
    procedure SetOnSaveNode(const Value: TCnTreeNodeEvent);
  {$ENDIF}
  {$IFDEF SUPPORT_FMX}
    function GetOnSaveItem: TCnTreeViewItemEvent;
    procedure SetOnSaveItem(const Value: TCnTreeViewItemEvent);
  {$ENDIF}
{$ENDIF}
{$ENDIF}
    function GetTotalCount: Integer;
    function GetItems(Index: Integer): TCnBerReadNode;
    function ParseArea(Parent: TCnLeaf; AData: PByteArray; ADataByteLen: Cardinal;
      AStartOffset: Cardinal; var IsEnd: Boolean; IsTop: Boolean = True;
      ADepth: Integer = 0; AMaxDataLen: Cardinal = 0): Cardinal;
    {* 解析一段数据为一个或多个节点，该数据里的所有 ASN.1 节点均序次挂在 Parent 节点下，
      返回这个 Area 的总长度。IsTop 表示是 Parent 是 Root 顶级节点，以处理长度问题。
      ADataLen 如果传 0，表示是不定长节点，ParseArea 此时需要判断 00 00 以告知上一级结尾了
      并通过 IsEnd 函数返回告诉调用者}
  protected

  public
    constructor Create(Data: PByte; DataByteLen: Cardinal; AParseInnerString: Boolean = False);
    {* 构造函数。

       参数：
         Data: PByte                      - 待解析的数据区地址
         DataByteLen: Cardinal            - 待解析的数据区字节长度
         AParseInnerString: Boolean       - 是否解析内嵌字符串

       返回值：TCnBerReader               - 返回创建的对象实例
    }

    destructor Destroy; override;
    {* 析构函数}

    procedure ParseToTree;
    {* 创建后需要调用此方法实施解析}
    procedure ManualParseNodeData(RootNode: TCnBerReadNode);
    {* 某些节点的 Tag 并非 SEQUENCE/SET 等但内容却有子内容，需要外部手工调用此方法来实施二次解析。

       参数：
         RootNode: TCnBerReadNode         - 待手工解析的节点

       返回值：（无）
    }

{$IFDEF DEBUG}
{$IFDEF ENABLE_UIINTERACT}
  {$IFDEF MSWINDOWS}
    procedure DumpToTreeView(ATreeView: ComCtrls.TTreeView); {$IFDEF SUPPORT_FMX} overload; {$ENDIF}
    {* 将内容输出至界面上的 VCL 树控件中。

       参数：
         ATreeView: ComCtrls.TTreeView    - 待输出内容的 VCL 树控件实例

       返回值：（无）
    }

    property OnSaveNode: TCnTreeNodeEvent read GetOnSaveNode write SetOnSaveNode;
  {$ENDIF}
  {$IFDEF SUPPORT_FMX}
    procedure DumpToTreeView(ATreeView: FMX.TreeView.TTreeView); {$IFDEF MSWINDOWS} overload; {$ENDIF}
    {* 将内容输出至界面上的 FMX 树控件中。

       参数：
         ATreeView: FMX.TreeView.TTreeView                - 待输出内容的 FMX 树控件实例

       返回值：（无）
    }

    property OnSaveItem: TCnTreeViewItemEvent read GetOnSaveItem write SetOnSaveItem;
  {$ENDIF}
{$ENDIF}
{$ENDIF}

    property ParseInnerString: Boolean read FParseInnerString;
    {* 是否将 BitString/OctetString 类型也当作复合类型来解析，PKCS#8 的 Pem 文件中常见}
    property TotalCount: Integer read GetTotalCount;
    {* 解析出来的 ASN.1 节点总数}
    property Items[Index: Integer]: TCnBerReadNode read GetItems;
    {* 顺序访问所有解析出来的 ASN.1 节点，下标从 0 开始，不包括 Tree 自身的 Root}
  end;

  TCnBerWriteNode = class(TCnLeaf)
  {* 描述一用于编码并写入的 ASN.1 节点}
  private
    FMem: TMemoryStream;        // 容纳基本类型节点的所有内容，不只是数据区
    FHead: array[0..5] of Byte; // 容纳数据区之前的头内容，包括 Tag、Len 等
    FHeadLen: Integer;
    FIsContainer: Boolean;
    FDataLength: Integer;
    FData: Pointer;
    FBerTag: Integer;
    FBerTypeMask: Byte;
    function GetIsContainer: Boolean;
    procedure SetIsContainer(const Value: Boolean);
    function GetItems(AIndex: Integer): TCnBerWriteNode;
    procedure SetItems(AIndex: Integer; const Value: TCnBerWriteNode);

    procedure FillHeadCalcLen(ATag: Integer; ADataLen: Integer);
    // 计算并填充 FHead 与 FHeadLen
  public
    constructor Create(ATree: TCnTree); override;
    {* 构造函数。

       参数：
         ATree: TCnTree                   - 指定所属树

       返回值：TCnBerWriteNode            - 返回创建的对象实例
    }

    destructor Destroy; override;
    {* 析构函数}

    function SaveToStream(Stream: TStream): Integer;
    {* 如果是基本类型就将自己写入流并返回写入字节长度，
       如果是容器则挨个让子节点写出来，然后加自己头来拼成流并拼各子节点的返回字节长度。
       返回值为本节点包括子节点的所有内容的写入字节字节长度。

       参数：
         Stream: TStream                  - 写入的流

       返回值：Integer                    - 本节点包括子节点的所有内容的写入字节长度
    }

    function SaveValueToStream(Stream: TStream): Integer;
    {* 如果是基本类型就将自己除了 Tag 与长度之外后面的数据内容写入流并返回写入字节长度。
       如果是容器则挨个让子节点写出来后返回。
       返回值为子节点所有内容长度但不包括自己的 Tag 与长度。

       参数：
         Stream: TStream                  - 写入的流

       返回值：Integer                    - 返回子节点所有内容的写入字节长度
    }

    function GetNodeLength: Integer;
    {* 如果是基本类型就返回自身字节长度，如果是容器则自己头加各子节点的字节长度。

       参数：
         （无）

       返回值：Integer                    - 返回字节长度
    }

    procedure FillBasicNode(ATag: Integer; AData: PByte; ADataByteLen: Integer);
    {* 外界创建此基本节点后用此方法填充基本数据，Container 节点不用，
       注意原始 BitString 不支持头字节，暂不需要自己填充。

       参数：
         ATag: Integer                    - 用于填充的 Tag
         AData: PByte                     - 用于填充的数据块地址
         ADataByteLen: Integer            - 用于填充的数据块字节长度

       返回值：（无）
    }

    property Items[AIndex: Integer]: TCnBerWriteNode read GetItems write SetItems;
    {* 索引子节点}
    property Data: Pointer read FData write FData;
    {* 数据块地址}
    property DataLength: Integer read FDataLength write FDataLength;
    {* 数据块字节长度}
    property IsContainer: Boolean read GetIsContainer write SetIsContainer;
    {* 是否是容器}
    property BerTag: Integer read FBerTag write FBerTag;
    {* 节点类型，也就是 Tag}
    property BerTypeMask: Byte read FBerTypeMask write FBerTypeMask;
    {* 节点 Mask，只有最高两位有效，写入 BerTag 时会与此值或}
  end;

  TCnBerWriter = class(TObject)
  {* 写 BER 编码的数据的工具类}
  private
    FBerTree: TCnTree;
    function GetTotalSize: Integer;
{$IFDEF DEBUG}
{$IFDEF ENABLE_UIINTERACT}
  {$IFDEF MSWINDOWS}
    function GetOnSaveNode: TCnTreeNodeEvent;
    procedure SetOnSaveNode(const Value: TCnTreeNodeEvent);
  {$ENDIF}
  {$IFDEF SUPPORT_FMX}
    function GetOnSaveItem: TCnTreeViewItemEvent;
    procedure SetOnSaveItem(const Value: TCnTreeViewItemEvent);
  {$ENDIF}
{$ENDIF}
{$ENDIF}
  public
    constructor Create;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure SaveTo(DestBuf: Pointer);
    {* 将 BER 内容保存至 DestBuf 所指的内存区，内存区大小至少需要 GetTotalSize。

       参数：
         DestBuf: Pointer                 - 容纳待输出内容的数据块地址

       返回值：（无）
    }

    procedure SaveToFile(const FileName: string);
    {* 将 BER 内容保存至指定文件。

       参数：
         const FileName: string           - 保存的文件名

       返回值：（无）
    }

    procedure SaveToStream(Stream: TStream);
    {* 将 BER 内容保存至流。

       参数：
         Stream: TStream                  - 保存的流

       返回值：（无）
    }

{$IFDEF DEBUG}
{$IFDEF ENABLE_UIINTERACT}
  {$IFDEF MSWINDOWS}
    procedure DumpToTreeView(ATreeView: ComCtrls.TTreeView); {$IFDEF SUPPORT_FMX} overload; {$ENDIF}
    {* 将内容输出至界面上的 VCL 树控件中。

       参数：
         ATreeView: ComCtrls.TTreeView    - 待输出内容的 VCL 树控件实例

       返回值：（无）
    }

    property OnSaveNode: TCnTreeNodeEvent read GetOnSaveNode write SetOnSaveNode;
  {$ENDIF}
  {$IFDEF SUPPORT_FMX}
    procedure DumpToTreeView(ATreeView: FMX.TreeView.TTreeView); {$IFDEF MSWINDOWS} overload; {$ENDIF}
    {* 将内容输出至界面上的 FMX 树控件中。

       参数：
         ATreeView: FMX.TreeView.TTreeView                - 待输出内容的 FMX 树控件实例

       返回值：（无）
    }

    property OnSaveItem: TCnTreeViewItemEvent read GetOnSaveItem write SetOnSaveItem;
  {$ENDIF}
{$ENDIF}
{$ENDIF}

    function AddNullNode(Parent: TCnBerWriteNode = nil): TCnBerWriteNode;
    {* 新增一个 Null 节点。

       参数：
         Parent: TCnBerWriteNode          - 待新增节点的父节点，如为 nil 表示新增至根节点下

       返回值：TCnBerWriteNode            - 返回新增的节点
    }

    function AddBasicNode(ATag: Integer; AData: PByte; ADataByteLen: Integer;
      Parent: TCnBerWriteNode = nil): TCnBerWriteNode; overload;
    {* 新增一个基本类型的节点，内容从 AData 复制长度为 DataLen 的而来

       参数：
         ATag: Integer                    - 待新增节点的 Tag
         AData: PByte                     - 待新增的数据块地址
         ADataByteLen: Integer            - 待新增的数据块字节长度
         Parent: TCnBerWriteNode          - 待新增节点的父节点，如为 nil 表示新增至根节点下

       返回值：TCnBerWriteNode            - 返回新增的节点
    }

    function AddBasicNode(ATag: Integer; AStream: TStream;
      Parent: TCnBerWriteNode = nil): TCnBerWriteNode; overload;
    {* 新增一个基本类型的节点，内容从指定流复制而来。

       参数：
         ATag: Integer                    - 待新增节点的 Tag
         AStream: TStream                 - 待新增的流内容
         Parent: TCnBerWriteNode          - 待新增节点的父节点，如为 nil 表示新增至根节点下

       返回值：TCnBerWriteNode            - 返回新增的节点
    }

    function AddAnsiStringNode(ATag: Integer; const AStr: AnsiString;
      Parent: TCnBerWriteNode = nil): TCnBerWriteNode;
    {* 新增一个字符串型的 Node，内容从指定 AnsiString 复制而来。

       参数：
         ATag: Integer                    - 待新增节点的 Tag
         const AStr: AnsiString           - 待新增的字符串
         Parent: TCnBerWriteNode          - 待新增节点的父节点，如为 nil 表示新增至根节点下

       返回值：TCnBerWriteNode            - 返回新增的节点
    }

    function AddContainerNode(ATag: Integer; Parent: TCnBerWriteNode = nil): TCnBerWriteNode;
    {* 新增一个容器类型的节点，此节点可以作为上面 BasicNode 的 Parent。

       参数：
         ATag: Integer                    - 待新增节点的 Tag
         Parent: TCnBerWriteNode          - 待新增节点的父节点，如为 nil 表示新增至根节点下

       返回值：TCnBerWriteNode            - 返回新增的节点
    }

    function AddRawNode(RawTag: Integer; RawLV: PByte; LVLen: Integer;
      Parent: TCnBerWriteNode = nil): TCnBerWriteNode;
    {* 新增一个原始节点，此节点的 Tag 值直接由 RawTag 指定，
       后面的长度内容等不计算了，直接由 RawLV 与 LVLen 的区域指定。

       参数：
         RawTag: Integer                  - 指定的原始 Tag
         RawLV: PByte                     - 指定的原始长度
         LVLen: Integer                   - 指定的原始长度字段所代表的长度
         Parent: TCnBerWriteNode          - 待新增节点的父节点，如为 nil 表示新增至根节点下

       返回值：TCnBerWriteNode            - 返回新增的节点
    }

    property TotalSize: Integer read GetTotalSize;
    {* 整棵树的总尺寸，字节为单位}
  end;

function CompareObjectIdentifier(Node: TCnBerReadNode; OIDAddr: Pointer;
  OIDSize: Integer): Boolean;
{* 比较一个节点中的数据是否等于一个指定的 OID。

   参数：
     Node: TCnBerReadNode                 - 待比较的节点
     OIDAddr: Pointer                     - OID 数据块地址
     OIDSize: Integer                     - OID 数据块字节长度

   返回值：Boolean                        - 返回是否相等
}

function AddBigNumberToWriter(Writer: TCnBerWriter; Num: TCnBigNumber;
  Parent: TCnBerWriteNode; Tag: Integer = CN_BER_TAG_INTEGER; FixedLen: Integer = 0): TCnBerWriteNode;
{* 将一个大数的内容写入一个新增的 Ber 整型格式的节点，FixedLen 为 0 时无固定长度，
   FixedLen 指定大数实际长度不足时使用固定长度，为 0 时则使用大数实际长度。
   如 FixedLen 为 0，节点会根据最高位的实际情况决定是否加一个字节 0。
   如 FixedLen 不为 0，节点会在 FixedLen 与大数实际长度的基础上强行加一个字节 0。

   参数：
     Writer: TCnBerWriter                 - BER 写入的工具类实例
     Num: TCnBigNumber                    - 待写入的大数
     Parent: TCnBerWriteNode              - 待新增的节点的父节点
     Tag: Integer                         - 指定写入的 Tag
     FixedLen: Integer                    - 写入固定字节长度，0 表示使用大数实际长度

   返回值：TCnBerWriteNode                - 返回新增的节点
}

function CnBerParseDateTime(const Value: AnsiString; BerTag: Integer;
  RequireDERCertificateFormat: Boolean = False): TDateTime;
{* 严格解析 ASN.1 UTCTime 或 GeneralizedTime，并返回对应的 UTC 时间。
   本函数不依赖操作系统区域设置，要求时间字符串被完整消费，且必须显式包含
   Z 或 +hhmm/-hhmm 时区。RequireDERCertificateFormat 为 True 时按 RFC 5280
   证书时间规则，只接受带秒并以 Z 结尾的规范 DER 格式。

   参数：
     const Value: AnsiString              - 待解析的 ASCII 时间字符串
     BerTag: Integer                      - CN_BER_TAG_UTCTIME 或 CN_BER_TAG_GENERALIZEDTIME
     RequireDERCertificateFormat: Boolean - 是否强制 RFC 5280 证书 DER 时间格式

   返回值：TDateTime                      - 解析并换算后的 UTC 日期时间

   异常：
     ECnBerException                      - Tag、语法、日期、时区或年份编码不合法
}

procedure PutIndexedBigIntegerToBigNumber(Node: TCnBerReadNode; BigNumber: TCnBigNumber);
{* 将一个 Ber 整型格式的节点内容写到一个大数中。

   参数：
     Node: TCnBerReadNode                 - 待写入的节点
     BigNumber: TCnBigNumber              - 待写入的大数

   返回值：（无）
}

implementation

resourcestring
  SCnErrorDataCorruptionTagBase = 'Data Corruption when Processing Tag (Base %d), %d > %d.';
  SCnErrorDataCorruptionTagBaseLen = 'Data Corruption when Processing Tag (Base %d) at %d Got Len %d.';
  SCnErrorLengthTooLongOrIncorrect = 'Length Too Long or Incorrect (Base %d) %d.';
  SCnErrorOffsetLenTag = 'Offset %d. Len %d. Tag %d (%s). DataLen %d';
  SCnErrorBerTagTypeMismatchForBytesize = 'Ber Tag Type Mismatch for ByteSize: ';
  SCnErrorInvalidBytesize = 'Invalid ByteSize: ';
  SCnErrorDataLengthOverflow = 'Data Length %d Overflow for Required %d.';
  SCnErrorBerTagTypeMismatch = 'Ber Tag Type Mismatch for Int64: ';
  SCnErrorBerTagTypeMismatchForString = 'Ber Tag Type Mismatch for String: ';
  SCnErrorBerTagTypeMismatchForBignumber = 'Ber Tag Type Mismatch for BigNumber.';
  SCnErrorBerTagTypeMismatchForBoolean = 'Ber Tag Type Mismatch for Boolean: ';
  SCnErrorBerTagTypeMismatchForCommonInteger = 'Ber Tag Type Mismatch for Common Integer.';
  SCnErrorDataLengthOverflowForCommonInteger = 'Data Length %d Overflow for Common Integer.';
  SCnErrorBerParseDepthOverflow = 'BER Parse Depth Overflow %d';
  SCnErrorBerDataLengthExceeds = 'BER Data Length Exceeds Available Buffer (Base %d, Offset %d, Len %d, Available %d).';
  SCnErrorBerInvalidTime = 'Invalid ASN.1 Time for Tag %d.';

const
  CN_TAG_SET_STRING: TCnBerTagSet = [CN_BER_TAG_UFT8STRING, CN_BER_TAG_NUMERICSTRING,
    CN_BER_TAG_PRINTABLESTRING, CN_BER_TAG_IA5STRING, CN_BER_TAG_TELETEXSTRING];

  CN_TAG_SET_TIME: TCnBerTagSet = [CN_BER_TAG_UTCTIME, CN_BER_TAG_GENERALIZEDTIME];

{$IFDEF DEBUG}

function GetTagName(Tag: Integer): string;
begin
  Result := 'Invalid';
  if Tag in [Ord(Low(TCnBerTag))..Ord(High(TCnBerTag))] then
  begin
    Result := GetEnumName(TypeInfo(TCnBerTag), Tag);
    if (Length(Result) > 3) and (Copy(Result, 1, 3) = 'cbt') then
      Delete(Result, 1, 3);
  end;
end;

{$ENDIF}

function CompareObjectIdentifier(Node: TCnBerReadNode; OIDAddr: Pointer;
  OIDSize: Integer): Boolean;
var
  P: Pointer;
begin
  Result := False;
  if (Node <> nil) then
  begin
    P := Node.BerDataAddress;
    if (P <> nil) and (OIDAddr <> nil) and (OIDSize > 0) then
    begin
      if OIDSize = Node.BerDataLength then
        Result := CompareMem(OIDAddr, P, OIDSize);
    end;
  end;
end;

// FixedLen 小于或等于 0 时，如果整数最高位是 1，则需要前面补 0 避免与负数的表述混淆
// FixedLen 大于 0 时，如够长，则固定补 0，否则按大数实际情况补 0
function CalcIntegerTLV(BigNumber: TCnBigNumber; FixedLen: Integer = 0): Cardinal;
begin
  Result := BigNumber.GetBytesCount;
  if FixedLen <= 0 then
  begin
    if BigNumber.IsBitSet((Result * 8) - 1) then // 根据最高位是否是 1 决定是否补 0
      Inc(Result);
  end
  else
  begin
    if Cardinal(FixedLen) >= Result then // 固定位，够长，前面补 0
      Result := FixedLen + 1
    else if BigNumber.IsBitSet((Result * 8) - 1) then // 固定位不够按实际长，前面按需补 0
      Inc(Result);
  end;
end;

function AddBigNumberToWriter(Writer: TCnBerWriter; Num: TCnBigNumber;
  Parent: TCnBerWriteNode; Tag: Integer; FixedLen: Integer): TCnBerWriteNode;
var
  P: Pointer;
  C, D: Integer;
begin
  Result := nil;
  if (Writer = nil) or (Num = nil) then
    Exit;

  // Integer 编码需要处理最高位以决定是否补一个 0
  C := CalcIntegerTLV(Num, FixedLen);
  if C <= 0 then
    Exit;

  P := GetMemory(C);
  D := C - Num.GetBytesCount;

  FillChar(P^, D, 0);
  Num.ToBinary(PAnsiChar(TCnIntAddress(P) + D));

  Result := Writer.AddBasicNode(Tag, P, C, Parent);
  FreeMemory(P);
end;

procedure PutIndexedBigIntegerToBigNumber(Node: TCnBerReadNode; BigNumber: TCnBigNumber);
var
  P: Pointer;
begin
  if (Node = nil) or (Node.BerDataLength <= 0) then
    Exit;

  P := GetMemory(Node.BerDataLength);
  Node.CopyDataTo(P);
  BigNumber.SetBinary(P, Node.BerDataLength);
  FreeMemory(P);
end;

{ TCnBerReader }

constructor TCnBerReader.Create(Data: PByte; DataByteLen: Cardinal;
  AParseInnerString: Boolean);
begin
  FData := Data;
  FDataByteLen := DataByteLen;
  FParseInnerString := AParseInnerString;
  FBerTree := TCnTree.Create(TCnBerReadNode);
end;

destructor TCnBerReader.Destroy;
begin
  FBerTree.Free;
  inherited;
end;

{$IFDEF DEBUG}
{$IFDEF ENABLE_UIINTERACT}
{$IFDEF MSWINDOWS}

procedure TCnBerReader.DumpToTreeView(ATreeView: ComCtrls.TTreeView);
begin
  FBerTree.SaveToTreeView(ATreeView);
end;

function TCnBerReader.GetOnSaveNode: TCnTreeNodeEvent;
begin
  Result := FBerTree.OnSaveANode;
end;

procedure TCnBerReader.SetOnSaveNode(const Value: TCnTreeNodeEvent);
begin
  FBerTree.OnSaveANode := Value;
end;

{$ENDIF}

{$IFDEF SUPPORT_FMX}

procedure TCnBerReader.DumpToTreeView(ATreeView: FMX.TreeView.TTreeView);
begin
  FBerTree.SaveToTreeView(ATreeView);
end;

function TCnBerReader.GetOnSaveItem: TCnTreeViewItemEvent;
begin
  Result := FBerTree.OnSaveAItem;
end;

procedure TCnBerReader.SetOnSaveItem(const Value: TCnTreeViewItemEvent);
begin
  FBerTree.OnSaveAItem := Value;
end;

{$ENDIF}
{$ENDIF}
{$ENDIF}

function TCnBerReader.GetItems(Index: Integer): TCnBerReadNode;
begin
  Result := TCnBerReadNode(FBerTree.Items[Index + 1]);
end;

function TCnBerReader.GetTotalCount: Integer;
begin
  Result := FBerTree.Root.AllCount;
end;

function TCnBerReader.ParseArea(Parent: TCnLeaf; AData: PByteArray;
  ADataByteLen: Cardinal; AStartOffset: Cardinal; var IsEnd: Boolean; IsTop: Boolean;
  ADepth: Integer; AMaxDataLen: Cardinal): Cardinal;
var
  Run, Start: Cardinal;
  MaxRun: Cardinal;
  Tag, DataLen, DataOffset, LenLen, Delta, SubLen: Integer;
  B: Byte;
  IsStruct, OutLenIsZero, MyEnd, LenSingle: Boolean;
  ALeaf: TCnBerReadNode;
  TagClass: Integer;
{$IFDEF DEBUG}
  TagName: string;
{$ENDIF}
begin
  // 抛出专用子类 ECnBerDepthOverflowError：外层递归解析的 except 分支依赖该类型
  // re-raise 以终止整个解析；若抛基类 ECnBerException，超限异常会被内层
  // "on E: ECnBerException do ;" 吞掉，导致深层子树被静默丢弃而非整体报错。
  if ADepth > CN_BER_MAX_DEPTH then
    raise ECnBerDepthOverflowError.CreateFmt(SCnErrorBerParseDepthOverflow, [ADepth]);

  Run := 0;  // Run 是基于 AData 起始处的偏移量
  Result := ADataByteLen;
  OutLenIsZero := ADataByteLen = 0;
  MyEnd := False;

  // Indefinite-length mode: compute max safe offset to prevent OOB read
  if ADataByteLen = 0 then
  begin
    if AMaxDataLen > 0 then
      MaxRun := AMaxDataLen
    else if FDataByteLen > AStartOffset then
      MaxRun := FDataByteLen - AStartOffset
    else
      MaxRun := 0;
  end
  else
    MaxRun := ADataByteLen;

  while (ADataByteLen = 0) or (Run < ADataByteLen) do // ADataLen 如果等于 0 表示是不定长节点
  begin
    // Boundary check in indefinite-length mode
    if (ADataByteLen = 0) and (Run >= MaxRun) then
      Exit;

    B := AData^[Run];

    if B = $FF then
      Exit;

    Start := Run;

    // 处理 Tag 类型
    IsStruct := (B and CN_BER_TAG_STRUCT_MASK) <> 0;
    Tag := B and CN_BER_TAG_VALUE_MASK;
    TagClass := B and CN_BER_TAG_TYPE_MASK;

    Inc(Run);
    if (Run >= ADataByteLen) and (ADataByteLen > 0) then
      raise ECnBerException.CreateFmt(SCnErrorDataCorruptionTagBase,
        [AStartOffset, Run, ADataByteLen]);

    // Indefinite-length boundary check before accessing AData^[Run]
    if (ADataByteLen = 0) and (Run >= MaxRun) then
      Exit;

    // Run 指向长度，如果 Tag 和长度都是 0，表示不定长内容的终结，不新建节点
    // 注意 ADataLen 可能因为是顶层节点，长度由外部传入，
    if (IsTop or (ADataByteLen = 0)) and (B = 0) and (AData^[Run] = 0) then
    begin
      if OutLenIsZero then // 加 Tag 和 0 长度俩字节
        Inc(Result, 2);
      IsEnd := True;
      Exit;
    end;

    // 处理长度
    Delta := 1;  // 1 表示 Tag 所占字节
    B := AData^[Run];
    if (B and CN_BER_LENLEN_MASK) = 0 then
    begin
      // 本字节就是长度
      DataLen := B;
      DataOffset := AStartOffset + Run + 1;
      Inc(Delta); // 加上长度的这一字节
      Inc(Run);   // Run 指向数据
      LenSingle := True;
    end
    else
    begin
      // 本字节高位为 1，表示长度的长度
      LenSingle := False;
      LenLen := B and CN_BER_LENGTH_MASK;
      Inc(Delta); // 加上长度的长度这一字节
      Inc(Run);   // Run 指向具体长度，如果 LenLen 为 0，则 Run 指向下一个 Area 开头

      // AData[Run] 到 AData[Run + LenLen - 1] 是长度
      if ((ADataByteLen > 0) and (Run + Cardinal(LenLen) - 1 >= ADataByteLen)) or
         ((ADataByteLen = 0) and (Run + Cardinal(LenLen) - 1 >= MaxRun)) then
        raise ECnBerException.CreateFmt(SCnErrorDataCorruptionTagBaseLen,
          [AStartOffset, Run, LenLen]);

      DataLen := 0;
      if LenLen = SizeOf(Byte) then
        DataLen := AData^[Run]
      else if LenLen = SizeOf(Word) then
        DataLen := (Cardinal(AData^[Run]) shl 8) or Cardinal(AData^[Run + 1])
      else if LenLen = 3 then
        DataLen := (Cardinal(AData^[Run]) shl 16) or
                   (Cardinal(AData^[Run + 1]) shl 8) or
                   Cardinal(AData^[Run + 2])
      else if LenLen = SizeOf(Cardinal) then
      begin
        // 最高位为 1 时长度必然超过 MaxInt，强转 Integer 会得到负数，
        // 负数长度会导致 Inc(Run, DataLen) 回退与后续解析越界，必须拒绝
        if AData^[Run] > $7F then
          raise ECnBerException.CreateFmt(SCnErrorLengthTooLongOrIncorrect, [AStartOffset, LenLen]);

        DataLen := Integer((Cardinal(AData^[Run]) shl 24) or
                   (Cardinal(AData^[Run + 1]) shl 16) or
                   (Cardinal(AData^[Run + 2]) shl 8) or
                   Cardinal(AData^[Run + 3]))
      end
      else if LenLen > SizeOf(Cardinal) then  // TODO: LenLen = 0 时是不定长编码，BER 中支持，以 00 00 结尾
        raise ECnBerException.CreateFmt(SCnErrorLengthTooLongOrIncorrect, [AStartOffset, LenLen]);

      DataOffset := AStartOffset + Run + Cardinal(LenLen);
      if LenLen > 0 then
      begin
        Inc(Delta, LenLen);
        Inc(Run, LenLen);   // Run 指向数据
      end;
    end;

    // 修复越界读：校验声明长度 DataLen 是否落在真实可解析范围内，
    // 防止声明长度超过实际缓冲区导致后续按长度越界读取
    if DataLen < 0 then
      raise ECnBerException.CreateFmt(SCnErrorLengthTooLongOrIncorrect, [AStartOffset, LenLen]);

    if ADataByteLen > 0 then
    begin
      // 定长模式：可用长度为真实缓冲 FDataByteLen 减去起始偏移
      if FDataByteLen < AStartOffset then
        raise ECnBerException.CreateFmt(SCnErrorBerDataLengthExceeds,
          [AStartOffset, Run, DataLen, 0]);
      if (Cardinal(DataLen) > FDataByteLen - AStartOffset) or
         (Run > (FDataByteLen - AStartOffset) - Cardinal(DataLen)) then
        raise ECnBerException.CreateFmt(SCnErrorBerDataLengthExceeds,
          [AStartOffset, Run, DataLen, FDataByteLen - AStartOffset]);
    end
    else
    begin
      // 不定长模式：声明长度不能超过允许解析的上限 MaxRun
      if (Cardinal(DataLen) > MaxRun) or (Run > MaxRun - Cardinal(DataLen)) then
        raise ECnBerException.CreateFmt(SCnErrorBerDataLengthExceeds,
          [AStartOffset, Run, DataLen, MaxRun]);
    end;

    // Tag, Len, DataOffset 都齐全了，Delta 是数据起始区与当前节点起始区的偏移
    if Parent = nil then
      Parent := FBerTree.Root;

    ALeaf := FBerTree.AddChild(Parent) as TCnBerReadNode;
    ALeaf.FOriginData := FData;

    ALeaf.BerOffset := AStartOffset + Start;
    ALeaf.BerLength := DataLen + Delta;
    ALeaf.BerTag := Tag;
    ALeaf.FBerTagClass := TagClass;
    ALeaf.BerDataLength := DataLen;  // 注意 DataLen 为 0 时，如 LenSingle 是 True，表示没东西，为 False 才表示未定长度
    ALeaf.BerDataOffset := DataOffset;

    if OutLenIsZero then
      Inc(Result, ALeaf.BerLength);

{$IFDEF DEBUG}
    if TagClass = 0 then
      TagName := GetTagName(ALeaf.BerTag)
    else
      TagName := '[' + IntToStr(ALeaf.BerTag) + ']';
    ALeaf.Text := Format(SCnErrorOffsetLenTag, [ALeaf.BerOffset,
      ALeaf.BerLength, ALeaf.BerTag, TagName, ALeaf.BerDataLength]);
{$ENDIF}

    SubLen := 0;
    // 有子节点时对长度的要求：(DataLen > 0) 或 (DataLen = 0 且 LenSingle 为 False)
    // 也就是说，单纯一个字节表示 DataLen 是 0，确实就表示没数据
    // 组合表示 0，才说明是无固定长度数据，有子节点且最后一个字节点以 00 00 结尾
    if (IsStruct or (FParseInnerString and (TagClass = 0) and (ALeaf.BerTag in [CN_BER_TAG_BIT_STRING,
      CN_BER_TAG_OCTET_STRING])))
      and ((DataLen > 0) or (DataLen = 0) and not LenSingle) then
    begin
      // 说明 BerDataOffset 到 BerDataLength 内可能有子节点
      try
        // 不定长 BIT STRING 的 DataLength 为 0，若走下方分支会把
        // BerDataLength - 1 下溢为巨大无符号数传给子解析导致越界读，
        // 故仅对确定长度的 BIT STRING 跳过首字节，不定长的按普通区域解析
        if (TagClass = 0) and (ALeaf.BerTag = CN_BER_TAG_BIT_STRING)
          and (ALeaf.BerDataLength >= 1) then
        begin
          // 凑成 8 的倍数所缺少的 Bit 数照理应小于 8，但不能加这个额外条件 and (AData^[Run + 1] < 8)
          FCurrentIsBitString := True;
          try
            try
              // BIT_STRING 数据区第一个内容字节是该 BIT_STRING 凑成 8 的倍数所缺少的 Bit 数，这里要跳过
              SubLen := ParseArea(ALeaf, PByteArray(TCnNativeUInt(AData) + Run + 1),
                ALeaf.BerDataLength - 1, ALeaf.BerDataOffset + 1, MyEnd, False,
                ADepth + 1, ALeaf.BerDataLength);
            except
              // 但有些场合没这个字节。所以上面出错时，不跳过这个字节，重新解析
              SubLen := ParseArea(ALeaf, PByteArray(TCnNativeUInt(AData) + Run),
                ALeaf.BerDataLength, ALeaf.BerDataOffset, MyEnd, False,
                ADepth + 1, ALeaf.BerDataLength);
            end;
          finally
            FCurrentIsBitString := False;
          end;
        end
        else
        begin
          SubLen := ParseArea(ALeaf, PByteArray(TCnNativeUInt(AData) + Run),
            ALeaf.BerDataLength, ALeaf.BerDataOffset, MyEnd, False,
            ADepth + 1, ALeaf.BerDataLength);
        end;
      except
        on E: ECnBerDepthOverflowError do
          raise; // 深度超限属于致命错误，必须终止整个解析，不得被吞掉
        on E: ECnBerException do
          ;      // 如果内嵌解析失败，不终止，当做普通节点（仅限 BER 解析类异常）
        else
          raise; // 内存访问违例等其他异常必须上抛，不得吞掉
      end;
    end;

    if DataLen = 0 then // 本轮的本块（不是外头总块）长度不确定时，步进时要走子解析返回的长度
      Inc(Run, SubLen)
    else
      Inc(Run, DataLen);

    Inc(Result, SubLen);  // 子块无论结束与否，其长度都要叠加到父长度上返回
  end;
end;

procedure TCnBerReader.ParseToTree;
var
  MyEnd: Boolean;
begin
  ParseArea(FBerTree.Root, PByteArray(FData), FDataByteLen, 0, MyEnd);
end;

procedure TCnBerReader.ManualParseNodeData(RootNode: TCnBerReadNode);
var
  MyEnd: Boolean;
begin
  RootNode.Clear;
  ParseArea(RootNode, PByteArray(RootNode.BerDataAddress), RootNode.BerDataLength,
    RootNode.BerDataOffset, MyEnd, False);
  // 注意 RootNode 一般不会是 Tree 的 Root，因而 IsTop 要传 False
end;

{ TCnBerReadNode }

function TCnBerReadNode.AsPrintableString: string;
begin
  Result := string(InternalAsString([CN_BER_TAG_PRINTABLESTRING]));
end;

function TCnBerReadNode.InternalAsInteger(ByteSize: Integer): Integer;
var
  IntValue: Integer;
begin
  if FBerTag <> CN_BER_TAG_INTEGER then
    raise ECnBerException.Create(SCnErrorBerTagTypeMismatchForBytesize + IntToStr(ByteSize));

  if not (ByteSize in [SizeOf(Byte)..SizeOf(Cardinal)]) then
    raise ECnBerException.Create(SCnErrorInvalidBytesize + IntToStr(ByteSize));

  if FBerDataLength > ByteSize then
    raise ECnBerException.CreateFmt(SCnErrorDataLengthOverflow,
      [FBerDataLength, ByteSize]);

  IntValue := 0;
  CopyDataTo(@IntValue);

  // Byte 不需交换，SmallInt 交换两位，Integer 交换四位
  if ByteSize = SizeOf(Word) then
    IntValue := Integer(UInt16NetworkToHost(Word(IntValue)))
  else if ByteSize = SizeOf(Cardinal) then
    IntValue := UInt32NetworkToHost(IntValue);
  Result := IntValue;
end;

function TCnBerReadNode.AsInt64: Int64;
begin
  if FBerTag <> CN_BER_TAG_INTEGER then
    raise ECnBerException.Create(SCnErrorBerTagTypeMismatch + IntToStr(FBerTag));

  if FBerDataLength > SizeOf(Int64) then
    raise ECnBerException.CreateFmt(SCnErrorDataLengthOverflow,
      [FBerDataLength, SizeOf(Int64)]);

  Result := 0;
  CopyDataTo(@Result);
  Result := Int64NetworkToHost(Result);
end;

function TCnBerReadNode.AsByte: Byte;
begin
  Result := Byte(InternalAsInteger(SizeOf(Byte)));
end;

function TCnBerReadNode.AsCardinal: Cardinal;
begin
  Result := Cardinal(InternalAsInteger(SizeOf(Cardinal)));
end;

function TCnBerReadNode.AsInteger: Integer;
begin
  Result := Integer(InternalAsInteger(SizeOf(Integer)));
end;

function TCnBerReadNode.AsShortInt: ShortInt;
begin
  Result := ShortInt(InternalAsInteger(SizeOf(ShortInt)));
end;

function TCnBerReadNode.AsSmallInt: SmallInt;
begin
  Result := SmallInt(InternalAsInteger(SizeOf(SmallInt)));
end;

function TCnBerReadNode.AsWord: Word;
begin
  Result := Word(InternalAsInteger(SizeOf(Word)));
end;

procedure TCnBerReadNode.CopyDataTo(DestBuf: Pointer);
begin
  if (FOriginData <> nil) and (FBerDataLength > 0) then
    Move(Pointer(TCnIntAddress(FOriginData) + FBerDataOffset)^, DestBuf^, FBerDataLength);
end;

function TCnBerReadNode.GetItems(AIndex: Integer): TCnBerReadNode;
begin
  Result := inherited GetItems(AIndex) as TCnBerReadNode;
end;

procedure TCnBerReadNode.SetItems(AIndex: Integer; const Value: TCnBerReadNode);
begin
  inherited SetItems(AIndex, Value);
end;

function TCnBerReadNode.GetBerDataAddress: Pointer;
begin
  if FOriginData = nil then
    Result := nil
  else
    Result := Pointer(TCnIntAddress(FOriginData) + FBerDataOffset);
end;

function TCnBerReadNode.GetNextSibling: TCnBerReadNode;
begin
  Result := TCnBerReadNode(inherited GetNextSibling);
end;

function TCnBerReadNode.GetPrevSibling: TCnBerReadNode;
begin
  Result := TCnBerReadNode(inherited GetPrevSibling);
end;

procedure TCnBerReadNode.CopyHeadTo(DestBuf: Pointer);
begin
  if FOriginData <> nil then
    Move(Pointer(TCnIntAddress(FOriginData) + FBerOffset)^, DestBuf^, FBerLength - FBerDataLength);
end;

procedure TCnBerReadNode.CopyTLVTo(DestBuf: Pointer);
begin
  if (FOriginData <> nil) and (FBerLength > 0) then
    Move(Pointer(TCnIntAddress(FOriginData) + FBerOffset)^, DestBuf^, FBerLength);
end;

function TCnBerReadNode.AsIA5String: string;
begin
  Result := string(InternalAsString([CN_BER_TAG_IA5STRING]));
end;

function TCnBerReadNode.AsString: string;
begin
  Result := string(InternalAsString(CN_TAG_SET_STRING + CN_TAG_SET_TIME));
end;

function InternalParseTimeDigits(const S: AnsiString; StartIndex,
  DigitCount: Integer; out Value: Integer): Boolean;
var
  I: Integer;
  C: AnsiChar;
begin
  Result := False;
  Value := 0;
  if (StartIndex < 1) or (DigitCount <= 0) or
    (StartIndex > Length(S) - DigitCount + 1) then
    Exit;

  for I := 0 to DigitCount - 1 do
  begin
    C := S[StartIndex + I];
    if (C < '0') or (C > '9') then
      Exit;
    Value := Value * 10 + Ord(C) - Ord('0');
  end;
  Result := True;
end;

procedure InternalRaiseInvalidBerTime(BerTag: Integer);
begin
  raise ECnBerException.CreateFmt(SCnErrorBerInvalidTime, [BerTag]);
end;

function CnBerParseDateTime(const Value: AnsiString; BerTag: Integer;
  RequireDERCertificateFormat: Boolean): TDateTime;
var
  L, BaseLen, ZoneSign: Integer;
  Year, Month, Day, Hour, Minute, Second: Integer;
  OffsetHour, OffsetMinute, OffsetMinutes: Integer;
  IsZulu: Boolean;
  LocalTime: TDateTime;
begin
  if (BerTag <> CN_BER_TAG_UTCTIME) and
    (BerTag <> CN_BER_TAG_GENERALIZEDTIME) then
    InternalRaiseInvalidBerTime(BerTag);

  L := Length(Value);
  if L <= 0 then
    InternalRaiseInvalidBerTime(BerTag);

  ZoneSign := 0;
  OffsetHour := 0;
  OffsetMinute := 0;
  IsZulu := Value[L] = 'Z';
  if IsZulu then
    BaseLen := L - 1
  else if (L >= 5) and ((Value[L - 4] = '+') or (Value[L - 4] = '-')) then
  begin
    BaseLen := L - 5;
    if Value[L - 4] = '+' then
      ZoneSign := 1
    else
      ZoneSign := -1;
    if not InternalParseTimeDigits(Value, L - 3, 2, OffsetHour) or
      not InternalParseTimeDigits(Value, L - 1, 2, OffsetMinute) or
      (OffsetHour > 23) or (OffsetMinute > 59) then
      InternalRaiseInvalidBerTime(BerTag);
  end
  else
  begin
    BaseLen := 0;
    InternalRaiseInvalidBerTime(BerTag);
  end;

  if BerTag = CN_BER_TAG_UTCTIME then
  begin
    if (BaseLen <> 10) and (BaseLen <> 12) then
      InternalRaiseInvalidBerTime(BerTag);
    if not InternalParseTimeDigits(Value, 1, 2, Year) then
      InternalRaiseInvalidBerTime(BerTag);
    if Year >= 50 then
      Inc(Year, 1900)
    else
      Inc(Year, 2000);
    if not InternalParseTimeDigits(Value, 3, 2, Month) or
      not InternalParseTimeDigits(Value, 5, 2, Day) or
      not InternalParseTimeDigits(Value, 7, 2, Hour) or
      not InternalParseTimeDigits(Value, 9, 2, Minute) then
      InternalRaiseInvalidBerTime(BerTag);
    if BaseLen = 12 then
    begin
      if not InternalParseTimeDigits(Value, 11, 2, Second) then
        InternalRaiseInvalidBerTime(BerTag);
    end
    else
      Second := 0;

    if RequireDERCertificateFormat and
      ((BaseLen <> 12) or not IsZulu) then
      InternalRaiseInvalidBerTime(BerTag);
  end
  else
  begin
    if (BaseLen <> 12) and (BaseLen <> 14) then
      InternalRaiseInvalidBerTime(BerTag);
    if not InternalParseTimeDigits(Value, 1, 4, Year) or
      not InternalParseTimeDigits(Value, 5, 2, Month) or
      not InternalParseTimeDigits(Value, 7, 2, Day) or
      not InternalParseTimeDigits(Value, 9, 2, Hour) or
      not InternalParseTimeDigits(Value, 11, 2, Minute) then
      InternalRaiseInvalidBerTime(BerTag);
    if BaseLen = 14 then
    begin
      if not InternalParseTimeDigits(Value, 13, 2, Second) then
        InternalRaiseInvalidBerTime(BerTag);
    end
    else
      Second := 0;

    if RequireDERCertificateFormat then
    begin
      if (BaseLen <> 14) or not IsZulu then
        InternalRaiseInvalidBerTime(BerTag);
      // RFC 5280 要求 1950 至 2049 使用 UTCTime，范围之外使用 GeneralizedTime。
      if (Year >= 1950) and (Year <= 2049) then
        InternalRaiseInvalidBerTime(BerTag);
    end;
  end;

  try
    LocalTime := EncodeDate(Year, Month, Day) +
      EncodeTime(Hour, Minute, Second, 0);
  except
    on E: Exception do
      raise ECnBerException.CreateFmt(SCnErrorBerInvalidTime, [BerTag]);
  end;

  OffsetMinutes := OffsetHour * 60 + OffsetMinute;
  if ZoneSign > 0 then
    Result := LocalTime - OffsetMinutes / 1440.0
  else if ZoneSign < 0 then
    Result := LocalTime + OffsetMinutes / 1440.0
  else
    Result := LocalTime;
end;

function TCnBerReadNode.AsDateTime: TDateTime;
begin
  Result := CnBerParseDateTime(InternalAsString(CN_TAG_SET_TIME), FBerTag,
    False);
end;

function TCnBerReadNode.InternalAsString(TagSet: TCnBerTagSet): AnsiString;
var
  P: Pointer;
begin
  if (TagSet <> []) and not (FBerTag in TagSet) then
    raise ECnBerException.Create(SCnErrorBerTagTypeMismatchForString + IntToStr(FBerTag));

  Result := '';
  P := GetBerDataAddress;
  if (P <> nil) and (BerDataLength > 0) then
  begin
    SetLength(Result, BerDataLength);
    Move(P^, Result[1], BerDataLength);
  end;
end;

function TCnBerReadNode.IsNull: Boolean;
begin
  Result := FBerTag = CN_BER_TAG_NULL;
end;

function TCnBerReadNode.IsDateTime: Boolean;
begin
  Result := FBerTag in CN_TAG_SET_TIME;
end;

function TCnBerReadNode.IsString: Boolean;
begin
  Result := FBerTag in (CN_TAG_SET_STRING + CN_TAG_SET_TIME);
end;

function TCnBerReadNode.IsInteger: Boolean;
begin
  Result := FBerTag = CN_BER_TAG_INTEGER;
end;

procedure TCnBerReadNode.AsBigNumber(OutNum: TCnBigNumber);
begin
  if FBerTag <> CN_BER_TAG_INTEGER then
    raise ECnBerException.Create(SCnErrorBerTagTypeMismatchForBignumber);

  OutNum.SetBinary(GetBerDataAddress, FBerDataLength);
end;

function TCnBerReadNode.GetBerAddress: Pointer;
begin
  if FOriginData = nil then
    Result := nil
  else
    Result := Pointer(TCnIntAddress(FOriginData) + FBerOffset);
end;

function TCnBerReadNode.AsBoolean: Boolean;
var
  B: Byte;
begin
  B := 0;
  if (FBerTag <> CN_BER_TAG_BOOLEAN) or (FBerDataLength <> 1) then
    raise ECnBerException.Create(SCnErrorBerTagTypeMismatchForBoolean + IntToStr(FBerTag));

  CopyDataTo(@B);
  Result := B <> 0;
end;

function TCnBerReadNode.AsRawString: string;
begin
  Result := string(InternalAsString([]));
end;

function TCnBerReadNode.AsAnsiString: AnsiString;
begin
  Result := InternalAsString([]);
end;

function TCnBerReadNode.AsCommonInteger: Integer;
var
  IntValue: Integer;
begin
  if FBerTag <> CN_BER_TAG_INTEGER then
    raise ECnBerException.Create(SCnErrorBerTagTypeMismatchForCommonInteger);

  if FBerDataLength > SizeOf(Cardinal) then
    raise ECnBerException.CreateFmt(SCnErrorDataLengthOverflowForCommonInteger,
      [FBerDataLength]);

  IntValue := 0;
  CopyDataTo(@IntValue);

  // Byte 不需交换，SmallInt 交换两位，Integer 交换四位
  if FBerDataLength = SizeOf(Word) then
    IntValue := Integer(UInt16NetworkToHost(Word(IntValue)))
  else if FBerDataLength = SizeOf(Cardinal) then
    IntValue := UInt32NetworkToHost(IntValue);
  Result := IntValue;
end;

{ TCnBerWriter }

function TCnBerWriter.AddBasicNode(ATag: Integer; AData: PByte;
  ADataByteLen: Integer; Parent: TCnBerWriteNode): TCnBerWriteNode;
begin
  if Parent = nil then
    Parent := FBerTree.Root as TCnBerWriteNode;

  Result := FBerTree.AddChild(Parent) as TCnBerWriteNode;
  Result.FIsContainer := False;
  Result.FillBasicNode(ATag, AData, ADataByteLen);
end;

function TCnBerWriter.AddContainerNode(ATag: Integer;
  Parent: TCnBerWriteNode): TCnBerWriteNode;
begin
  if Parent = nil then
    Parent := FBerTree.Root as TCnBerWriteNode;

  Result := FBerTree.AddChild(Parent) as TCnBerWriteNode;
  Result.BerTag := ATag;
  Result.IsContainer := True;
end;

function TCnBerWriter.AddNullNode(Parent: TCnBerWriteNode): TCnBerWriteNode;
begin
  if Parent = nil then
    Parent := FBerTree.Root as TCnBerWriteNode;

  Result := FBerTree.AddChild(Parent) as TCnBerWriteNode;
  Result.IsContainer := False;
  Result.FillBasicNode(CN_BER_TAG_NULL, nil, 0); // TODO: Null 的数据
end;

constructor TCnBerWriter.Create;
begin
  inherited;
  FBerTree := TCnTree.Create(TCnBerWriteNode);
end;

destructor TCnBerWriter.Destroy;
begin
  FBerTree.Free;
  inherited;
end;

{$IFDEF DEBUG}
{$IFDEF ENABLE_UIINTERACT}
{$IFDEF MSWINDOWS}

procedure TCnBerWriter.DumpToTreeView(ATreeView: ComCtrls.TTreeView);
begin
  FBerTree.SaveToTreeView(ATreeView);
end;

function TCnBerWriter.GetOnSaveNode: TCnTreeNodeEvent;
begin
  Result := FBerTree.OnSaveANode;
end;

procedure TCnBerWriter.SetOnSaveNode(const Value: TCnTreeNodeEvent);
begin
  FBerTree.OnSaveANode := Value;
end;

{$ENDIF}

{$IFDEF SUPPORT_FMX}

procedure TCnBerWriter.DumpToTreeView(ATreeView: FMX.TreeView.TTreeView);
begin
  FBerTree.SaveToTreeView(ATreeView);
end;

function TCnBerWriter.GetOnSaveItem: TCnTreeViewItemEvent;
begin
  Result := FBerTree.OnSaveAItem;
end;

procedure TCnBerWriter.SetOnSaveItem(const Value: TCnTreeViewItemEvent);
begin
  FBerTree.OnSaveAItem := Value;
end;

{$ENDIF}
{$ENDIF}
{$ENDIF}

function TCnBerWriter.GetTotalSize: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FBerTree.Root.Count - 1 do
    Result := Result + TCnBerWriteNode(FBerTree.Root).Items[I].GetNodeLength;
end;

procedure TCnBerWriter.SaveTo(DestBuf: Pointer);
var
  Mem: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  try
    SaveToStream(Mem);
    Mem.Position := 0;
    Mem.Read(DestBuf^, Mem.Size);
  finally
    Mem.Free;
  end;
end;

procedure TCnBerWriter.SaveToFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCnBerWriter.SaveToStream(Stream: TStream);
var
  I: Integer;
begin
  for I := 0 to FBerTree.Root.Count - 1 do
    TCnBerWriteNode(FBerTree.Root).Items[I].SaveToStream(Stream);
end;

function TCnBerWriter.AddRawNode(RawTag: Integer; RawLV: PByte;
  LVLen: Integer; Parent: TCnBerWriteNode): TCnBerWriteNode;
var
  B: Byte;
begin
  if Parent = nil then
    Parent := FBerTree.Root as TCnBerWriteNode;

  Result := FBerTree.AddChild(Parent) as TCnBerWriteNode;
  Result.BerTag := RawTag;

  B := RawTag;
  Result.FMem.Write(B, 1);
  if (RawLV <> nil) and (LVLen > 0) then
    Result.FMem.Write(RawLV^, LVLen);
end;

function TCnBerWriter.AddBasicNode(ATag: Integer; AStream: TStream;
  Parent: TCnBerWriteNode): TCnBerWriteNode;
var
  Mem: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  try
    Mem.LoadFromStream(AStream);
    Result := AddBasicNode(ATag, Mem.Memory, Mem.Size, Parent);
  finally
    Mem.Free;
  end;
end;

function TCnBerWriter.AddAnsiStringNode(ATag: Integer;
  const AStr: AnsiString; Parent: TCnBerWriteNode): TCnBerWriteNode;
begin
  if Parent = nil then
    Parent := FBerTree.Root as TCnBerWriteNode;
  
  Result := FBerTree.AddChild(Parent) as TCnBerWriteNode;
  Result.FIsContainer := False;
  Result.FillBasicNode(ATag, PByte(AStr), Length(AStr));
end;

{ TCnBerWriteNode }

procedure TCnBerWriteNode.FillHeadCalcLen(ATag: Integer; ADataLen: Integer);
var
  LenLen: Cardinal;
  B: Byte;
  W: Word;
  D: Cardinal;
begin
  FHeadLen := 0;
  if FIsContainer and (FBerTag in [CN_BER_TAG_SEQUENCE, CN_BER_TAG_SET]) then
    FHead[0] := ATag or CN_BER_TAG_STRUCT_MASK // 有子节点且是指定类型，高位置 1
  else if FIsContainer and ((FBerTypeMask and CN_BER_TAG_TYPE_MASK) <> 0) then // 有特殊 Mask 时允许其他类型的 Tag 做 Container
    FHead[0] := ATag or CN_BER_TAG_STRUCT_MASK or (FBerTypeMask and CN_BER_TAG_TYPE_MASK)
  else
    FHead[0] := ATag;

  Inc(FHeadLen);
  if (FBerTag = CN_BER_TAG_BIT_STRING) and ((FBerTypeMask and CN_BER_TAG_TYPE_MASK) = 0) then
    Inc(ADataLen);

  if ADataLen <= 127 then // 单字节长度
  begin
    FHead[1] := ADataLen;
    Inc(FHeadLen);
  end
  else
  begin
    // 大于或等于 128，先求 LeafLen 的字节数
    if ADataLen < $100 then
    begin
      LenLen := 1;
      B := ADataLen;
      Move(B, FHead[2], LenLen);
    end
    else if ADataLen < $10000 then
    begin
      LenLen := 2;
      W := ADataLen;
      W := UInt16HostToNetwork(W);
      Move(W, FHead[2], LenLen);
    end
    else if ADataLen < $1000000 then
    begin
      LenLen := 3;
      D := ADataLen;
      D := UInt32HostToNetwork(D);
      D := D shr 8;
      Move(D, FHead[2], LenLen);
    end
    else
    begin
      LenLen := 4;
      D := ADataLen;
      D := UInt32HostToNetwork(D);
      Move(D, FHead[2], LenLen);
    end;

    FHead[1] := CN_BER_LENLEN_MASK or LenLen;
    Inc(FHeadLen, 1 + LenLen);
  end;
end;

constructor TCnBerWriteNode.Create(ATree: TCnTree);
begin
  inherited;
  FMem := TMemoryStream.Create;
end;

destructor TCnBerWriteNode.Destroy;
begin
  FMem.Free;
  inherited;
end;

function TCnBerWriteNode.GetIsContainer: Boolean;
begin
  Result := FIsContainer;
end;

function TCnBerWriteNode.GetItems(AIndex: Integer): TCnBerWriteNode;
begin
  Result := TCnBerWriteNode(inherited GetItems(AIndex));
end;

function TCnBerWriteNode.SaveToStream(Stream: TStream): Integer;
var
  B: Byte;
  I: Integer;
  LeafLen: Cardinal;
  AMem: TMemoryStream;
begin
  if FIsContainer then
  begin
    LeafLen := 0;
    Result := 0;
    AMem := TMemoryStream.Create;
    try
      for I := 0 to Count - 1 do
        LeafLen := LeafLen + Cardinal(Items[I].SaveToStream(AMem));

      FillHeadCalcLen(FBerTag, LeafLen);
      // 把 Tag、LeafLen 以及 AMem 的数据组合后写入

      Result := Result + Stream.Write(FHead[0], FHeadLen); // 写头与长度
      if (FBerTag = CN_BER_TAG_BIT_STRING) and ((FBerTypeMask and CN_BER_TAG_TYPE_MASK) = 0) then
      begin
        B := 0;
        Result := Result + Stream.Write(B, 1);
      end;

      // 写具体内容
      Result := Result + Stream.Write(AMem.Memory^, AMem.Size);
    finally
      AMem.Free;
    end;
  end
  else
  begin
    Result := Stream.Write(FMem.Memory^, FMem.Size);
  end;
end;

procedure TCnBerWriteNode.SetIsContainer(const Value: Boolean);
begin
  FIsContainer := Value;
end;

procedure TCnBerWriteNode.SetItems(AIndex: Integer;
  const Value: TCnBerWriteNode);
begin
  inherited SetItems(AIndex, Value);
end;

function TCnBerWriteNode.GetNodeLength: Integer;
var
  I, LeafLen: Integer;
begin
  if FIsContainer then
  begin
    LeafLen := 0;
    for I := 0 to Count - 1 do
      LeafLen := LeafLen + Items[I].GetNodeLength;

    FillHeadCalcLen(FBerTag, LeafLen);
    Result := FHeadLen + LeafLen;

    // BitString 需要一个前导字节表示补足的 bit
    if (FBerTag = CN_BER_TAG_BIT_STRING) and ((FBerTypeMask and CN_BER_TAG_TYPE_MASK) = 0) then
      Inc(Result);
  end
  else
  begin
    Result := FMem.Size;
  end;
end;

procedure TCnBerWriteNode.FillBasicNode(ATag: Integer; AData: PByte;
  ADataByteLen: Integer);
var
  B: Byte;
begin
  FBerTag := ATag;
  if FIsContainer then
    Exit;

  FData := AData;
  FDataLength := ADataByteLen;
  FillHeadCalcLen(ATag, ADataByteLen);

  FMem.Clear;
  FMem.Write(FHead[0], FHeadLen);
  if ADataByteLen > 0 then
  begin
    // 纯 BitString 需要补一个前导字节，头长度已经在 FillHeadCalcLen 内补上了
    if (ATag = CN_BER_TAG_BIT_STRING) and ((FBerTypeMask and CN_BER_TAG_TYPE_MASK) = 0) then
    begin
      B := 0;
      FMem.Write(B, 1);
    end;
    FMem.Write(Data^, ADataByteLen);
  end;
end;

function TCnBerWriteNode.SaveValueToStream(Stream: TStream): Integer;
var
  B: Byte;
  I: Integer;
  AMem: TMemoryStream;
begin
  Result := 0;
  if FIsContainer then
  begin
    AMem := TMemoryStream.Create;
    try
      for I := 0 to Count - 1 do
        Items[I].SaveToStream(AMem);

      if (FBerTag = CN_BER_TAG_BIT_STRING) and ((FBerTypeMask and CN_BER_TAG_TYPE_MASK) = 0) then
      begin
        B := 0;
        Result := Result + Stream.Write(B, 1);
      end;

      // 写具体内容
      Result := Result + Stream.Write(AMem.Memory^, AMem.Size);
    finally
      AMem.Free;
    end;
  end
  else
  begin
    if (FHeadLen > 0) and (FMem.Size > FHeadLen) then
      Result := Stream.Write(Pointer(TCnIntAddress(FMem.Memory) + FHeadLen)^, FMem.Size - FHeadLen);
  end;
end;

end.
