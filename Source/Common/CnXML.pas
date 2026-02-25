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

unit CnXML;
{* |<PRE>
================================================================================
* 软件名称: CnPack 组件包
* 单元名称: XML 解析器和包装单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元实现了一个独立的 XML 解析库，不依赖 MSXML。支持 XML 文档解析、
*           操作和序列化功能。
*
*           词法分析器内部使用 AnsiString 处理 UTF8 编码的 XML 文本，
*           外部接口字符串类型根据编译器版本自动适配。
*
* 开发平台：PWin7Pro + Delphi 5.01
* 兼容测试：PWin7/10+ Delphi 5~最新、FPC
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2026.01.15 V1.0
*                创建单元，在 AI 的帮助下实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, TypInfo;

type
{$IFNDEF TBYTES_DEFINED}
  TBytes = array of Byte;
{$ENDIF}

//==============================================================================
// Exception and Type Definitions
//==============================================================================

  ECnXMLException = class(Exception)
  {* XML 异常类}
  private
    FErrorCode: Integer;
    FLine: Integer;
    FColumn: Integer;
  public
    constructor Create(const Msg: string; AErrorCode, ALine, AColumn: Integer);
    {* 构造函数。

       参数：
         const Msg: string                    - 错误信息
         AErrorCode: Integer                  - 错误码
         ALine: Integer                       - 错误所在行号
         AColumn: Integer                     - 错误所在列号

       返回值：（无）
    }

    property ErrorCode: Integer read FErrorCode;
    {* 错误码}
    property Line: Integer read FLine;
    {* 异常所在行号}
    property Column: Integer read FColumn;
    {* 异常所在列号}
  end;

  TCnXMLNodeType = (
  {* XML 节点类型枚举}
    xntElement,           // Element node
    xntAttribute,         // Attribute node
    xntText,              // Text node
    xntCData,             // CDATA node
    xntEntityReference,   // Entity reference node
    xntEntity,            // Entity node
    xntPI,                // Processing instruction node
    xntComment,           // Comment node
    xntDocument,          // Document node
    xntDocumentType,      // Document type node
    xntDocumentFragment,  // Document fragment node
    xntNotation           // Notation node
  );

  TCnXMLTokenType = (
  {* XML 词法单元类型枚举}
    xttNone,              // Invalid token
    xttXMLDecl,           // XML declaration <?xml ... ?>
    xttStartTag,          // Start tag <tag>
    xttEndTag,            // End tag </tag>
    xttEmptyTag,          // Self-closing tag <tag/>
    xttText,              // Text content
    xttCData,             // CDATA section <![CDATA[...]]>
    xttComment,           // Comment <!-- ... -->
    xttPI,                // Processing instruction <?target data?>
    xttEOF                // End of file
  );

//==============================================================================
// Error Code Constants
//==============================================================================

const
  // Lexical analysis errors
  CN_XML_ERR_INVALID_CHAR = 1;           // Invalid character
  {* 词法分析错误：无效字符}
  CN_XML_ERR_UNEXPECTED_EOF = 2;         // Unexpected end of file
  {* 词法分析错误：意外的文件结束}
  CN_XML_ERR_INVALID_NAME = 3;           // Invalid name
  {* 词法分析错误：非法名称}
  CN_XML_ERR_MISSING_QUOTE = 4;          // Missing quote
  {* 词法分析错误：缺少引号}
  CN_XML_ERR_INVALID_ENTITY = 5;         // Invalid entity reference
  {* 词法分析错误：非法实体引用}

  // Syntax analysis errors
  CN_XML_ERR_TAG_MISMATCH = 10;          // Tag mismatch
  {* 语法分析错误：标签不匹配}
  CN_XML_ERR_MISSING_ROOT = 11;          // Missing root element
  {* 语法分析错误：缺少根元素}
  CN_XML_ERR_MULTIPLE_ROOTS = 12;        // Multiple root elements
  {* 语法分析错误：存在多个根元素}
  CN_XML_ERR_INVALID_STRUCTURE = 13;     // Invalid structure
  {* 语法分析错误：结构非法}

  // DOM operation errors
  CN_XML_ERR_HIERARCHY = 20;             // Hierarchy error
  {* DOM 操作错误：层级关系非法}
  CN_XML_ERR_NOT_FOUND = 21;             // Node not found
  {* DOM 操作错误：未找到节点}
  CN_XML_ERR_INVALID_OPERATION = 22;     // Invalid operation
  {* DOM 操作错误：非法操作}

  // Encoding errors
  CN_XML_ERR_ENCODING = 30;              // Encoding error
  {* 编码错误：编码处理失败}
  CN_XML_ERR_INVALID_ENCODING = 31;      // Invalid encoding
  {* 编码错误：不支持的编码}

  // Encoding constants
  CN_XML_ENCODING_UNKNOWN = 0;
  {* 编码常量：未知}
  CN_XML_ENCODING_GBK = 1;
  {* 编码常量：GBK}
  CN_XML_ENCODING_UTF8 = 2;
  {* 编码常量：UTF-8}
  CN_XML_ENCODING_UTF16LE = 3;
  {* 编码常量：UTF-16LE}
  CN_XML_ENCODING_UTF16BE = 4;
  {* 编码常量：UTF-16BE}

  // Collection serialization constants
  CN_XML_COLLECTION_ITEM_NODE = 'o';     // Compatible format
  {* 集合序列化常量：集合项节点名}

//==============================================================================
// String Constants
//==============================================================================

resourcestring
  SCN_XML_INVALID_CHAR = 'XML Parsing Error: Invalid Character';
  {* XML 解析错误：无效字符}
  SCN_XML_UNEXPECTED_EOF = 'XML Parsing Error: Unexpected End of Cile';
  {* XML 解析错误：意外的文件结束}
  SCN_XML_INVALID_NAME = 'XML Parsing Error: Invalid Name';
  {* XML 解析错误：非法名称}
  SCN_XML_MISSING_QUOTE = 'XML Parsing Error: Missing Quote';
  {* XML 解析错误：缺少引号}
  SCN_XML_INVALID_ENTITY = 'XML Parsing Error: Invalid Entity Reference';
  {* XML 解析错误：非法实体引用}
  SCN_XML_TAG_MISMATCH = 'XML Parsing Error: Tag Mismatch';
  {* XML 解析错误：标签不匹配}
  SCN_XML_MISSING_ROOT = 'XML Parsing Error: Missing Root Element';
  {* XML 解析错误：缺少根元素}
  SCN_XML_MULTIPLE_ROOTS = 'XML Parsing Error: Multiple Root Elements';
  {* XML 解析错误：存在多个根元素}
  SCN_XML_INVALID_STRUCTURE = 'XML Parsing Error: Invalid Structure';
  {* XML 解析错误：结构非法}
  SCN_XML_HIERARCHY = 'XML Parsing Error: Hierarchy Error';
  {* XML 解析错误：层级关系非法}
  SCN_XML_NOT_FOUND = 'XML Parsing Error: Node Not Found';
  {* XML 解析错误：未找到节点}
  SCN_XML_INVALID_OPERATION = 'XML Parsing Error: Invalid Operation';
  {* XML 解析错误：非法操作}
  SCN_XML_ENCODING = 'XML Encoding Error';
  {* XML 编码错误}
  SCN_XML_INVALID_ENCODING = 'XML Parsing Error: Invalid Encoding';
  {* XML 解析错误：不支持的编码}

type
//==============================================================================
// Forward Declarations
//==============================================================================

  TCnXMLNode = class;
  TCnXMLDocument = class;
  TCnXMLElement = class;
  TCnXMLAttribute = class;

//==============================================================================
// Lexical Analyzer
//==============================================================================

  TCnXMLToken = record
  {* XML Token 类型结构}
    TokenType: TCnXMLTokenType;
    {* Token 类型}
    Value: string;
    {* Token 文本}
    Line: Integer;
    {* 所在行}
    Column: Integer;
    {* 所在列}
    Attributes: TStringList;
    {* 属性列表}
  end;

  TCnXMLLexer = class
  {* XML 词法分析器}
  private
    FSource: string;          // Source XML text
    FPosition: Integer;       // Current position
    FLine: Integer;           // Current line number
    FColumn: Integer;         // Current column number
    FCurrentChar: Char;       // Current character
    FLength: Integer;         // Source text length (for optimization)

    procedure NextChar;       // Move to next character
    procedure SkipWhitespace; // Skip whitespace characters
    function PeekChar(Offset: Integer): Char;  // Peek character
    function ReadName: string;                 // Read XML name
    function ReadAttributeValue: string;       // Read attribute value
    function ReadText: string;                 // Read text content
    function ReadComment: string;              // Read comment
    function ReadCData: string;                // Read CDATA
    function ReadPI: string;                   // Read processing instruction
  public
    constructor Create(const ASource: string);
    {* 构造函数。

       参数：
         const ASource: string            - XML 源文本

       返回值：（无）
    }

    destructor Destroy; override;
    {* 析构函数}

    function NextToken: TCnXMLToken;
    {* 获取下一个词法单元。

       参数：
         （无）

       返回值：TCnXMLToken                - 返回词法单元记录
    }

    function CurrentPosition: Integer;
    {* 获取当前解析位置。

       参数：
         无

       返回值：Integer                    - 当前字符位置
    }

    function CurrentLine: Integer;
    {* 获取当前行号。

       参数：
         无

       返回值：Integer                    - 当前行号
    }

    function CurrentColumn: Integer;
    {* 获取当前列号。

       参数：
         无

       返回值：Integer                    - 当前列号
    }

    function UnescapeText(const Text: string): string;
    {* 反转义实体引用文本。

       参数：
         const Text: string               - 待反转义文本

       返回值：string                     - 反转义后的文本
    }
  end;

//==============================================================================
// DOM Base Classes
//==============================================================================

  TCnXMLNode = class
  {* XML 节点类}
  private
    FNodeType: TCnXMLNodeType;
    FNodeValue: string;
    FParentNode: TCnXMLNode;
    FOwnerDocument: TCnXMLDocument;
    FChildNodes: TList;  // List of child nodes

    function GetFirstChild: TCnXMLNode;
    function GetLastChild: TCnXMLNode;
    function GetNextSibling: TCnXMLNode;
    function GetPreviousSibling: TCnXMLNode;
    function GetChildCount: Integer;
    function GetChild(Index: Integer): TCnXMLNode;
    function GetText: string;
    procedure SetText(const Value: string);
  protected
    FNodeName: string;
    procedure AddChild(Node: TCnXMLNode);
    procedure InternalRemoveChild(Node: TCnXMLNode);
    function IndexOfChild(Node: TCnXMLNode): Integer;
  public
    constructor Create(AOwnerDocument: TCnXMLDocument; ANodeType: TCnXMLNodeType);
    {* 构造函数。

       参数：
         AOwnerDocument: TCnXMLDocument   - 所属文档对象
         ANodeType: TCnXMLNodeType        - 节点类型

       返回值：（无）
    }

    destructor Destroy; override;
    {* 析构函数}

    // Node operations
    function AppendChild(NewChild: TCnXMLNode): TCnXMLNode;
    {* 追加子节点。

       参数：                        
         NewChild: TCnXMLNode             - 待追加的子节点

       返回值：TCnXMLNode                 - 返回被追加的子节点
    }

    function InsertBefore(NewChild, RefChild: TCnXMLNode): TCnXMLNode;
    {* 在参考节点前插入子节点。

       参数：
         NewChild: TCnXMLNode             - 待插入的子节点
         RefChild: TCnXMLNode             - 参考子节点

       返回值：TCnXMLNode                 - 返回被插入的子节点
    }

    function RemoveChild(OldChild: TCnXMLNode): TCnXMLNode;
    {* 移除子节点。

       参数：
         OldChild: TCnXMLNode             - 待移除的子节点

       返回值：TCnXMLNode                 - 返回被移除的子节点
    }

    function ReplaceChild(NewChild, OldChild: TCnXMLNode): TCnXMLNode;
    {* 替换子节点。

       参数：
         NewChild: TCnXMLNode             - 新子节点
         OldChild: TCnXMLNode             - 旧子节点

       返回值：TCnXMLNode                 - 返回被替换的旧子节点
    }

    function CloneNode(Deep: Boolean): TCnXMLNode;
    {* 克隆节点。

       参数：
         Deep: Boolean                    - 是否递归克隆子节点

       返回值：TCnXMLNode                 - 返回克隆得到的新节点
    }

    function HasChildNodes: Boolean;
    {* 判断是否存在子节点。

       参数：
         （无）

       返回值：Boolean                    - 存在子节点返回 True，否则返回 False
    }

    // Properties
    property NodeType: TCnXMLNodeType read FNodeType;
    {* 节点类型}
    property NodeName: string read FNodeName write FNodeName;
    {* 节点名称}
    property NodeValue: string read FNodeValue write FNodeValue;
    {* 节点值}
    property ParentNode: TCnXMLNode read FParentNode;
    {* 父节点}
    property OwnerDocument: TCnXMLDocument read FOwnerDocument;
    {* 所属文档}
    property FirstChild: TCnXMLNode read GetFirstChild;
    {* 第一个子节点}
    property LastChild: TCnXMLNode read GetLastChild;
    {* 最后一个子节点}
    property NextSibling: TCnXMLNode read GetNextSibling;
    {* 下一个同级节点}
    property PreviousSibling: TCnXMLNode read GetPreviousSibling;
    {* 上一个同级节点}
    property ChildCount: Integer read GetChildCount;
    {* 子节点数量}
    property Children[Index: Integer]: TCnXMLNode read GetChild;
    {* 根据索引访问子节点}
    property Text: string read GetText write SetText;
    {* 节点文本内容}
  end;

  TCnXMLAttribute = class
  {* XML 属性类}
  private
    FName: string;
    FValue: string;
    FOwnerElement: TCnXMLElement;
  public
    constructor Create(AOwnerElement: TCnXMLElement; const AName, AValue: string);
    {* 构造函数。

       参数：
         AOwnerElement: TCnXMLElement     - 所属元素
         const AName: string              - 属性名
         const AValue: string             - 属性值

       返回值：（无）
    }

    property Name: string read FName write FName;
    {* 属性名}
    property Value: string read FValue write FValue;
    {* 属性值}
    property OwnerElement: TCnXMLElement read FOwnerElement;
    {* 所属元素}
  end;

  TCnXMLElement = class(TCnXMLNode)
  {* XML 元素类}
  private
    FAttributes: TList;  // List of attributes

    function GetAttributeCount: Integer;
    function GetAttributeName(Index: Integer): string;
    function GetAttributeValue(Index: Integer): string;
  public
    constructor Create(AOwnerDocument: TCnXMLDocument; const ATagName: string);
    {* 构造函数。

       参数：
         AOwnerDocument: TCnXMLDocument   - 所属文档对象
         const ATagName: string           - 标签名

       返回值：（无）
    }

    destructor Destroy; override;
    {* 析构函数}

    // Attribute operations
    function GetAttribute(const Name: string): string;
    {* 获取指定属性值。

       参数：
         const Name: string               - 属性名

       返回值：string                     - 属性值，未找到则返回空字符串
    }

    procedure SetAttribute(const Name, Value: string);
    {* 设置属性值。

       参数：
         const Name: string               - 属性名
         const Value: string              - 属性值

       返回值：（无）
    }

    function HasAttribute(const Name: string): Boolean;
    {* 判断是否存在指定属性。

       参数：
         const Name: string               - 属性名

       返回值：Boolean                    - 存在返回 True，否则返回 False
    }

    procedure RemoveAttribute(const Name: string);
    {* 删除指定属性。

       参数：
         const Name: string               - 属性名

       返回值：（无）
    }

    function GetAttributeNode(const Name: string): TCnXMLAttribute;
    {* 获取指定属性节点。

       参数：
         const Name: string               - 属性名

       返回值：TCnXMLAttribute            - 属性节点，未找到则返回 nil
    }

    // Element query
    function GetElementsByTagName(const TagName: string): TList;
    {* 按标签名查询子孙元素。

       参数：
         const TagName: string            - 标签名

       返回值：TList                      - 元素列表（调用者不用时请释放列表对象，但不要释放元素）
    }

    // Properties
    property TagName: string read FNodeName write FNodeName;
    {* 标签名}
    property AttributeCount: Integer read GetAttributeCount;
    {* 属性数量}
    property AttributeNames[Index: Integer]: string read GetAttributeName;
    {* 按索引获取属性名}
    property AttributeValues[Index: Integer]: string read GetAttributeValue;
    {* 按索引获取属性值}
  end;

  TCnXMLDocument = class(TCnXMLNode)
  {* XML 文档类}
  private
    FDocumentElement: TCnXMLElement;
    FEncoding: string;
    FVersion: string;
    FStandalone: string;
    FPreserveWhitespace: Boolean;
  public
    constructor Create;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    // Node creation
    function CreateElement(const TagName: string): TCnXMLElement;
    {* 创建元素节点。

       参数：
         const TagName: string            - 标签名

       返回值：TCnXMLElement              - 返回新创建的元素节点
    }

    function CreateTextNode(const Data: string): TCnXMLNode;
    {* 创建文本节点。

       参数：
         const Data: string               - 文本内容

       返回值：TCnXMLNode                 - 返回新创建的文本节点
    }

    function CreateComment(const Data: string): TCnXMLNode;
    {* 创建注释节点。

       参数：
         const Data: string               - 注释内容

       返回值：TCnXMLNode                 - 返回新创建的注释节点
    }

    function CreateCDATASection(const Data: string): TCnXMLNode;
    {* 创建 CDATA 节点。

       参数：
         const Data: string               - CDATA 内容

       返回值：TCnXMLNode                 - 返回新创建的 CDATA 节点
    }

    function CreateProcessingInstruction(const Target, Data: string): TCnXMLNode;
    {* 创建处理指令节点。

       参数：
         const Target: string             - 指令目标
         const Data: string               - 指令数据

       返回值：TCnXMLNode                 - 返回新创建的处理指令节点
    }

    // Override AppendChild to auto-set DocumentElement
    function AppendChild(NewChild: TCnXMLNode): TCnXMLNode;
    {* 追加子节点并更新根元素。

       参数：
         NewChild: TCnXMLNode             - 待追加的子节点

       返回值：TCnXMLNode                 - 返回被追加的子节点
    }

    // Document loading
    procedure LoadFromFile(const FileName: string);
    {* 从文件加载 XML。

       参数：
         const FileName: string           - 文件名

       返回值：（无）
    }

    procedure LoadFromStream(Stream: TStream);
    {* 从流加载 XML。

       参数：
         Stream: TStream                  - 输入流

       返回值：（无）
    }

    procedure LoadFromString(const XMLString: string);
    {* 从字符串加载 XML。

       参数：
         const XMLString: string          - XML 字符串

       返回值：（无）
    }

    // Document saving
    procedure SaveToFile(const FileName: string; Indent: Boolean);
    {* 保存 XML 到文件。

       参数：
         const FileName: string           - 文件名
         Indent: Boolean                  - 是否缩进输出

       返回值：（无）
    }

    procedure SaveToStream(Stream: TStream; Indent: Boolean);
    {* 保存 XML 到流、

       参数：
         Stream: TStream                  - 输出流
         Indent: Boolean                  - 是否缩进输出

       返回值：（无）
    }

    function SaveToString(Indent: Boolean): string;
    {* 保存 XML 为字符串。

       参数：
         Indent: Boolean                  - 是否缩进输出

       返回值：string                     - XML 字符串
    }

    // Properties
    property DocumentElement: TCnXMLElement read FDocumentElement write FDocumentElement;
    {* 文档根元素}
    property Encoding: string read FEncoding write FEncoding;
    {* 文档编码}
    property Version: string read FVersion write FVersion;
    {* XML 版本号}
    property Standalone: string read FStandalone write FStandalone;
    {* Standalone 声明值}
    property PreserveWhitespace: Boolean read FPreserveWhitespace write FPreserveWhitespace;
    {* 是否保留空白字符}
  end;

//==============================================================================
// Syntax Analyzer (Parser)
//==============================================================================

  TCnXMLParser = class
  {* XML 语法分析器}
  private
    FLexer: TCnXMLLexer;
    FDocument: TCnXMLDocument;
    FCurrentToken: TCnXMLToken;

    procedure NextToken;                             // Get next token
    procedure RaiseError(const Msg: string);         // Raise parsing error
    procedure ParseXMLDecl;                          // Parse XML declaration
    procedure ParseElement(ParentNode: TCnXMLNode);  // Parse element
    procedure ParseContent(ParentNode: TCnXMLNode);  // Parse content
  public
    constructor Create(const ASource: string);
    {* 构造函数。

       参数：
         const ASource: string            - XML 源文本

       返回值：无                         - 构造函数无返回值
    }

    destructor Destroy; override;
    {* 析构函数}

    function Parse: TCnXMLDocument;
    {* 执行语法解析并返回文档对象。

       参数：
         无

       返回值：TCnXMLDocument             - 解析得到的文档对象
    }
  end;

//==============================================================================
// Encoding Functions
//==============================================================================

function CnXMLDetectEncoding(const Buffer: TBytes): Integer;
{* 从字节缓冲区检测编码

   参数：
     const Buffer: TBytes                 - 字节缓冲区

   返回值：Integer                        - 编码常量（CN_XML_ENCODING_*）
}

function CnXMLConvertEncoding(const Source: AnsiString;
  SourceEncoding, TargetEncoding: Integer): AnsiString;
{* 转换编码（内部使用 AnsiString）

   参数：
     const Source: AnsiString             - 源字符串
     SourceEncoding: Integer              - 源编码常量
     TargetEncoding: Integer              - 目标编码常量

   返回值：AnsiString                     - 转换后的字符串
}

//==============================================================================
// Object Serialization Classes
//==============================================================================

type
  TCnXMLWriter = class(TComponent)
  {* 将对象写至 XML 的序列化实现类}
  private
    FDocument: TCnXMLDocument;
    FRootNode: TCnXMLElement;
    FEncoding: string;
    FOwnsDocument: Boolean;
    FUseDataNode: Boolean;
    procedure WriteProperties(const Obj: TPersistent; Node: TCnXMLElement);
    procedure WriteCollection(const Collection: TCollection; Node: TCnXMLElement);
    procedure SetClassType(const Obj: TPersistent; Node: TCnXMLElement);
    procedure SetUseDataNode(const Value: Boolean);
  protected
    function GetXMLString: string;
    procedure SetXMLString(const Value: string);
    procedure InitDocument;
  public
    constructor Create(AOwner: TComponent); override;
    {* 构造函数

       参数：
         AOwner: TComponent                    - 所属组件

       返回值：（无）
    }

    destructor Destroy; override;
    {* 析构函数}

    procedure WriteObjectToXML(const Obj: TPersistent; const RootName: string = '');
    {* 将对象序列化为 XML

       参数：
         const Obj: TPersistent                - 待序列化对象
         const RootName: string                - 根节点名，为空则使用 Obj.ClassName

       返回值：（无）
    }

    procedure SaveToFile(const FileName: string; Indent: Boolean = True);
    {* 保存 XML 到文件

       参数：
         const FileName: string               - 文件名
         Indent: Boolean                      - 是否缩进输出

       返回值：（无）
    }

    property Document: TCnXMLDocument read FDocument;
    {* XML 文档对象}
    property RootNode: TCnXMLElement read FRootNode;
    {* 根节点}
  published
    property Encoding: string read FEncoding write FEncoding;
    {* XML 编码}
    property XMLString: string read GetXMLString write SetXMLString;
    {* XML 字符串}
    property UseDataNode: Boolean read FUseDataNode write SetUseDataNode default True;
    {* 是否使用 <data> 包装节点（默认 True，兼容旧格式）}
  end;

  TCnXMLReader = class(TComponent)
  {* 从 XML 加载对象的反序列化实现类}
  private
    FDocument: TCnXMLDocument;
    FRootNode: TCnXMLElement;
    FEncoding: string;
    FOwnsDocument: Boolean;
    procedure ReadProperties(const Obj: TPersistent; Node: TCnXMLElement);
    procedure ReadCollection(Collection: TCollection; Node: TCnXMLElement);
    procedure SetPropertyValue(Obj: TPersistent; const PropName, PropValue: string; PProp: PPropInfo);
  protected
    function GetXMLString: string;
    procedure SetXMLString(const Value: string);
    procedure FindRootNode;
  public
    constructor Create(AOwner: TComponent); override;
    {* 构造函数。

       参数：
         AOwner: TComponent               - 所属组件

       返回值：（无）
    }

    destructor Destroy; override;
    {* 析构函数}

    function ReadObjectFromXML(Obj: TPersistent; const RootName: string = ''): Boolean;
    {* 从 XML 反序列化到对象。

       参数：
         Obj: TPersistent                 - 目标对象
         const RootName: string           - 根节点名，为空则使用 Obj.ClassName

       返回值：Boolean                    - 成功返回 True，否则返回 False
    }
    procedure LoadFromFile(const FileName: string);
    {* 从文件加载 XML

       参数：
         const FileName: string           - 文件名

       返回值：（无）
    }

    property Document: TCnXMLDocument read FDocument;
    {* XML 文档对象}
    property RootNode: TCnXMLElement read FRootNode;
    {* 根节点}
  published
    property Encoding: string read FEncoding write FEncoding;
    {* XML 编码}
    property XMLString: string read GetXMLString write SetXMLString;
    {* XML 字符串}
  end;

//==============================================================================
// Utility Functions
//==============================================================================

function CnXMLCreateDocument: TCnXMLDocument;
{* 创建 XML 文档对象

   参数：
     （无）

   返回值：TCnXMLDocument                 - 新建的文档对象
}

//==============================================================================
// String Conversion Helper Functions (for compatibility)
//==============================================================================

function CnXMLStrToRealDef(const S: string; Default: Extended): Extended;
{* 将字符串转换为实数（带默认值），XML 专用。

   参数：
     const S: string                      - 待转换字符串
     Default: Extended                    - 转换失败时的默认值

   返回值：Extended                       - 转换结果或默认值
}

function CnXMLStrToDateTime(const S: string; var Value: TDateTime): Boolean;
{* 将字符串转换为日期时间，XML 专用。

   参数：
     const S: string                      - 待转换字符串
     var Value: TDateTime                 - 输出日期时间

   返回值：Boolean                        - 转换成功返回 True，否则返回 False
}

function CnXMLStrToInt(const S: string; var Value: Integer): Boolean;
{* 将字符串转换为整数，XML 专用。

   参数：
     const S: string                      - 待转换字符串
     var Value: Integer                   - 输出整数值

   返回值：Boolean                        - 转换成功返回 True，否则返回 False
}

function CnXMLStrToIntDef(const S: string; Default: Integer): Integer;
{* 将字符串转换为整数（带默认值），XML 专用。

   参数：
     const S: string                      - 待转换字符串
     Default: Integer                     - 转换失败时的默认值

   返回值：Integer                        - 转换结果或默认值
}

function CnXMLStrToBool(const S: string; var Value: Boolean): Boolean;
{* 将字符串转换为布尔值（支持 0/1 与 True/False），XML 专用。

   参数：
     const S: string                      - 待转换字符串
     var Value: Boolean                   - 输出布尔值

   返回值：Boolean                        - 转换成功返回 True，否则返回 False
}

function CnXMLStrToInt64(const S: string; var Value: Int64): Boolean;
{* 将字符串转换为 Int64，XML 专用。

   参数：
     const S: string                      - 待转换字符串
     var Value: Int64                     - 输出 Int64 值

   返回值：Boolean                        - 转换成功返回 True，否则返回 False
}

function CnXMLIntToStr(Value: Integer): string;
{* 将整数转换为字符串，XML 专用。

   参数：
     Value: Integer                       - 整数值

   返回值：string                         - 字符串表示
}

function CnXMLBoolToStr(Value: Boolean): string;
{* 将布尔值转换为字符串，XML 专用。

   参数：
     Value: Boolean                       - 布尔值

   返回值：string                         - 字符串表示（True 或 False）
}

function CnXMLRealToStr(Value: Extended): string;
{* 将实数转换为字符串，XML 专用。

   参数：
     Value: Extended                      - 实数值

   返回值：string                         - 字符串表示
}

function CnXMLInt64ToStr(Value: Int64): string;
{* 将 Int64 转换为字符串，XML 专用。

   参数：
     Value: Int64                         - Int64 值

   返回值：string                         - 字符串表示
}

function CnXMLDateTimeToStrEx(Value: TDateTime): string;
{* 将日期时间转换为字符串（扩展格式），XML 专用。

   参数：
     Value: TDateTime                     - 日期时间值

   返回值：string                         - 字符串表示
}

implementation

{$IFNDEF UNICODE}
uses
  CnWideStrings;
{$ENDIF}

//==============================================================================
// ECnXMLException Implementation
//==============================================================================

constructor ECnXMLException.Create(const Msg: string; AErrorCode, ALine, AColumn: Integer);
begin
  inherited Create(Msg);
  FErrorCode := AErrorCode;
  FLine := ALine;
  FColumn := AColumn;
end;

//==============================================================================
// TCnXMLLexer Implementation
//==============================================================================

constructor TCnXMLLexer.Create(const ASource: string);
{$IFDEF UNICODE}
var
  I: Integer;
{$ENDIF}
begin
  inherited Create;
  FSource := ASource;
  FLength := Length(FSource);
  FPosition := 0;
  FLine := 1;
  FColumn := 0;

  // Skip BOM if present
{$IFDEF UNICODE}
  // In Unicode Delphi, string is UnicodeString (UTF-16)
  // Check for UTF-16 BOM (FEFF/FFFE for LE/BE) or UTF-8 BOM that wasn't stripped
  if (FLength >= 1) and (FSource[1] = #$FEFF) then
    FPosition := 1
  else if (FLength >= 1) and (FSource[1] = #$FFFE) then
  begin
    // UTF-16 BE, Exchange each WideChar
    for I := 2 to FLength do
      FSource[I] := Char(Swap(Ord(FSource[I])));
  end
  else if (FLength >= 3) and
    (FSource[1] = #$EF) and (FSource[2] = #$BB) and (FSource[3] = #$BF) then
    FPosition := 3;
{$ELSE}
  // In non-Unicode Delphi/FPC, string is AnsiString
  // Check for UTF-8 BOM (EF BB BF)
  if (FLength >= 3) and
     (FSource[1] = #$EF) and
     (FSource[2] = #$BB) and
     (FSource[3] = #$BF) then
  begin
    FPosition := 3;  // Skip UTF-8 BOM
  end;
{$ENDIF}

  NextChar;  // Initialize current character
end;

destructor TCnXMLLexer.Destroy;
begin
  inherited;
end;

procedure TCnXMLLexer.NextChar;
begin
  if FPosition <= FLength then
  begin
    if FPosition > 0 then
    begin
      // Update line and column numbers
      if FCurrentChar = #10 then
      begin
        Inc(FLine);
        FColumn := 0;
      end
      else if FCurrentChar <> #13 then
        Inc(FColumn);
    end;

    Inc(FPosition);
    if FPosition <= FLength then
      FCurrentChar := FSource[FPosition]
    else
      FCurrentChar := #0;  // End of file
  end;
end;

function TCnXMLLexer.PeekChar(Offset: Integer): Char;
var
  Pos: Integer;
begin
  Pos := FPosition + Offset;
  if (Pos > 0) and (Pos <= FLength) then
    Result := FSource[Pos]
  else
    Result := #0;
end;

function TCnXMLLexer.CurrentPosition: Integer;
begin
  Result := FPosition;
end;

function TCnXMLLexer.CurrentLine: Integer;
begin
  Result := FLine;
end;

function TCnXMLLexer.CurrentColumn: Integer;
begin
  Result := FColumn;
end;

procedure TCnXMLLexer.SkipWhitespace;
begin
  while (FCurrentChar <> #0) and (FCurrentChar <= ' ') do
    NextChar;
end;

function TCnXMLLexer.ReadName: string;
var
  StartPos: Integer;
  Ch: Char;
begin
  // XML name can start with letter (including Unicode), underscore, or colon
  // Subsequent characters can be letter, digit, underscore, colon, dot, or hyphen
  Result := '';

  Ch := FCurrentChar;

  // First character must be letter (ASCII or Unicode), underscore, or colon
  // For Unicode support, accept any character > #127 as potential letter
  if not ((Ch >= 'A') and (Ch <= 'Z')) and
     not ((Ch >= 'a') and (Ch <= 'z')) and
     (Ch <> '_') and (Ch <> ':') and
     (Ch <= #127) then
    Exit;

  StartPos := FPosition;

  // Read name: letter (ASCII or Unicode), digit, underscore, colon, dot, or hyphen
  while True do
  begin
    Ch := FCurrentChar;

    // Accept ASCII letters, digits, and special XML name characters
    if ((Ch >= 'A') and (Ch <= 'Z')) or
       ((Ch >= 'a') and (Ch <= 'z')) or
       ((Ch >= '0') and (Ch <= '9')) or
       (Ch = '_') or (Ch = ':') or (Ch = '.') or (Ch = '-') or
       (Ch > #127) then  // Accept Unicode characters
      NextChar
    else
      Break;
  end;

  Result := Copy(FSource, StartPos, FPosition - StartPos);
end;

function TCnXMLLexer.ReadText: string;
var
  StartPos: Integer;
begin
  // Read text content until '<' character
  Result := '';

  if FCurrentChar = '<' then
    Exit;

  StartPos := FPosition;

  while (FCurrentChar <> #0) and (FCurrentChar <> '<') do
    NextChar;

  Result := Copy(FSource, StartPos, FPosition - StartPos);

  // Note: We preserve whitespace to match others behavior
  // If you want to trim whitespace, use Trim(node.Text) in your code
end;

function TCnXMLLexer.ReadAttributeValue: string;
var
  QuoteChar: Char;
  StartPos: Integer;
begin
  // Read attribute value enclosed in double or single quotes
  Result := '';

  SkipWhitespace;

  if not (FCurrentChar in ['"', '''']) then
    Exit;

  QuoteChar := FCurrentChar;
  NextChar;  // Skip opening quote

  StartPos := FPosition;

  // Read until closing quote
  while (FCurrentChar <> #0) and (FCurrentChar <> QuoteChar) do
    NextChar;

  if FCurrentChar = #0 then
    Exit;  // Missing closing quote

  Result := Copy(FSource, StartPos, FPosition - StartPos);
  NextChar;  // Skip closing quote
end;

function TCnXMLLexer.UnescapeText(const Text: string): string;
var
  I, Len: Integer;
  Ch: Char;
  EntityStr: string;
  EntityValue: string;
  CharCode: Integer;
  IsHex: Boolean;
begin
  Result := '';
  Len := Length(Text);
  I := 1;

  while I <= Len do
  begin
    Ch := Text[I];

    if Ch = '&' then
    begin
      // Process entity reference
      EntityStr := '';
      Inc(I);

      // Read entity name
      while (I <= Len) and (Text[I] <> ';') do
      begin
        EntityStr := EntityStr + Text[I];
        Inc(I);
      end;

      if (I <= Len) and (Text[I] = ';') then
      begin
        Inc(I);  // Skip ';'

        // Parse entity reference
        if EntityStr = 'lt' then
          EntityValue := '<'
        else if EntityStr = 'gt' then
          EntityValue := '>'
        else if EntityStr = 'amp' then
          EntityValue := '&'
        else if EntityStr = 'quot' then
          EntityValue := '"'
        else if EntityStr = 'apos' then
          EntityValue := ''''
        else if (Length(EntityStr) > 1) and (EntityStr[1] = '#') then
        begin
          // Numeric character reference
          IsHex := False;

          if (Length(EntityStr) > 2) and (EntityStr[2] in ['x', 'X']) then
          begin
            // Hexadecimal character reference
            IsHex := True;
            EntityStr := Copy(EntityStr, 3, Length(EntityStr) - 2);
          end
          else
          begin
            // Decimal character reference
            EntityStr := Copy(EntityStr, 2, Length(EntityStr) - 1);
          end;

          try
            if IsHex then
              CharCode := StrToInt('$' + EntityStr)
            else
              CharCode := StrToInt(EntityStr);

            if (CharCode >= 0) and (CharCode <= 255) then
              EntityValue := Chr(CharCode)
            else
              EntityValue := '?';  // Invalid character
          except
            EntityValue := '?';  // Conversion failed
          end;
        end
        else
          EntityValue := '&' + EntityStr + ';';  // Unknown entity, keep original

        Result := Result + EntityValue;
      end
      else
      begin
        // Missing ';', keep original
        Result := Result + '&' + EntityStr;
      end;
    end
    else
    begin
      Result := Result + Ch;
      Inc(I);
    end;
  end;
end;

function TCnXMLLexer.ReadComment: string;
var
  StartPos: Integer;
begin
  // Read comment: <!-- ... -->
  // When this method is called, '<!--' has already been consumed
  Result := '';

  StartPos := FPosition;

  // Find '-->'
  while FCurrentChar <> #0 do
  begin
    if (FCurrentChar = '-') and (PeekChar(1) = '-') and (PeekChar(2) = '>') then
    begin
      Result := Copy(FSource, StartPos, FPosition - StartPos);
      NextChar;  // Skip '-'
      NextChar;  // Skip '-'
      NextChar;  // Skip '>'
      Break;
    end;
    NextChar;
  end;
end;

function TCnXMLLexer.ReadCData: string;
var
  StartPos: Integer;
begin
  // Read CDATA section: <![CDATA[...]]>
  // When this method is called, '<![CDATA[' has already been consumed
  Result := '';

  StartPos := FPosition;

  // Find ']]>'
  while FCurrentChar <> #0 do
  begin
    if (FCurrentChar = ']') and (PeekChar(1) = ']') and (PeekChar(2) = '>') then
    begin
      Result := Copy(FSource, StartPos, FPosition - StartPos);
      NextChar;  // Skip ']'
      NextChar;  // Skip ']'
      NextChar;  // Skip '>'
      Break;
    end;
    NextChar;
  end;
end;

function TCnXMLLexer.ReadPI: string;
var
  StartPos: Integer;
begin
  // Read processing instruction: <?target data?>
  // When this method is called, '<?' has already been consumed
  Result := '';

  StartPos := FPosition;

  // Find '?>'
  while FCurrentChar <> #0 do
  begin
    if (FCurrentChar = '?') and (PeekChar(1) = '>') then
    begin
      Result := Copy(FSource, StartPos, FPosition - StartPos);
      NextChar;  // Skip '?'
      NextChar;  // Skip '>'
      Break;
    end;
    NextChar;
  end;
end;

function TCnXMLLexer.NextToken: TCnXMLToken;
var
  TagName: string;
  AttrName, AttrValue: string;
  SavedPosition: Integer;
  SavedLine: Integer;
  SavedColumn: Integer;
  SavedChar: Char;
begin
  // Initialize token
  Result.TokenType := xttNone;
  Result.Value := '';
  Result.Line := FLine;
  Result.Column := FColumn;
  Result.Attributes := nil;

  // Save current position before skipping whitespace
  SavedPosition := FPosition;
  SavedLine := FLine;
  SavedColumn := FColumn;
  SavedChar := FCurrentChar;

  // Skip whitespace
  SkipWhitespace;

  // End of file
  if FCurrentChar = #0 then
  begin
    Result.TokenType := xttEOF;
    Exit;
  end;

  // Structures starting with '<'
  if FCurrentChar = '<' then
  begin
    NextChar;

    // Comment: <!-- ... -->
    if (FCurrentChar = '!') and (PeekChar(1) = '-') and (PeekChar(2) = '-') then
    begin
      NextChar;  // Skip '!'
      NextChar;  // Skip '-'
      NextChar;  // Skip '-'
      Result.TokenType := xttComment;
      Result.Value := ReadComment;
      Exit;
    end;

    // CDATA section: <![CDATA[...]]>
    if (FCurrentChar = '!') and (PeekChar(1) = '[') and
       (PeekChar(2) = 'C') and (PeekChar(3) = 'D') and
       (PeekChar(4) = 'A') and (PeekChar(5) = 'T') and
       (PeekChar(6) = 'A') and (PeekChar(7) = '[') then
    begin
      NextChar;  // Skip '!'
      NextChar;  // Skip '['
      NextChar;  // Skip 'C'
      NextChar;  // Skip 'D'
      NextChar;  // Skip 'A'
      NextChar;  // Skip 'T'
      NextChar;  // Skip 'A'
      NextChar;  // Skip '['
      Result.TokenType := xttCData;
      Result.Value := ReadCData;
      Exit;
    end;

    // Processing instruction: <?...?>
    if FCurrentChar = '?' then
    begin
      NextChar;  // Skip '?'

      // Check and parse XML declaration
      if (FCurrentChar = 'x') and (PeekChar(1) = 'm') and (PeekChar(2) = 'l') and
         (PeekChar(3) <= ' ') then
      begin
        NextChar;  // Skip 'x'
        NextChar;  // Skip 'm'
        NextChar;  // Skip 'l'
        Result.TokenType := xttXMLDecl;
        Result.Value := 'xml';
        Result.Attributes := TStringList.Create;

        // Parse XML declaration attributes
        SkipWhitespace;
        while (FCurrentChar <> #0) and (FCurrentChar <> '?') do
        begin
          AttrName := ReadName;
          if AttrName = '' then Break;

          SkipWhitespace;
          if FCurrentChar = '=' then
          begin
            NextChar;
            AttrValue := ReadAttributeValue;
            Result.Attributes.Add(AttrName + '=' + AttrValue);
          end;
          SkipWhitespace;
        end;

        // Skip '?>'
        if FCurrentChar = '?' then
          NextChar;
        if FCurrentChar = '>' then
          NextChar;
        Exit;
      end
      else
      begin
        // General processing instruction
        Result.TokenType := xttPI;
        Result.Value := ReadPI;
        Exit;
      end;
    end;

    // End tag: </tag>
    if FCurrentChar = '/' then
    begin
      NextChar;  // Skip '/'
      TagName := ReadName;
      Result.TokenType := xttEndTag;
      Result.Value := TagName;

      SkipWhitespace;
      if FCurrentChar = '>' then
        NextChar;
      Exit;
    end;

    // Start tag or self-closing tag: <tag> or <tag/>
    TagName := ReadName;
    if TagName <> '' then
    begin
      Result.Value := TagName;
      Result.Attributes := TStringList.Create;

      // Parse attributes
      SkipWhitespace;
      while (FCurrentChar <> #0) and (FCurrentChar <> '>') and (FCurrentChar <> '/') do
      begin
        AttrName := ReadName;
        if AttrName = '' then
        begin
          // If ReadName returns empty, skip whitespace and try again
          // This handles cases where there are extra whitespace characters
          SkipWhitespace;
          if (FCurrentChar = #0) or (FCurrentChar = '>') or (FCurrentChar = '/') then
            Break;
          // If still not at end, something is wrong, break anyway
          Break;
        end;

        SkipWhitespace;
        if FCurrentChar = '=' then
        begin
          NextChar;
          AttrValue := ReadAttributeValue;
          Result.Attributes.Add(AttrName + '=' + AttrValue);
        end;
        SkipWhitespace;
      end;

      // Check if self-closing tag
      if FCurrentChar = '/' then
      begin
        Result.TokenType := xttEmptyTag;
        NextChar;  // Skip '/'
        if FCurrentChar = '>' then
          NextChar;
      end
      else if FCurrentChar = '>' then
      begin
        Result.TokenType := xttStartTag;
        NextChar;
      end
      else
      begin
        // If we don't find '>' or '/', still treat as start tag
        // This handles malformed XML more gracefully
        Result.TokenType := xttStartTag;
      end;
      Exit;
    end;
  end;

  // Text content
  if FCurrentChar <> '<' then
  begin
    // Restore position to before SkipWhitespace to preserve leading whitespace
    FPosition := SavedPosition;
    FLine := SavedLine;
    FColumn := SavedColumn;
    FCurrentChar := SavedChar;

    Result.TokenType := xttText;
    Result.Value := ReadText;
    // Decode entity references
    Result.Value := UnescapeText(Result.Value);
    Exit;
  end;
end;

//==============================================================================
// TCnXMLNode Implementation
//==============================================================================

constructor TCnXMLNode.Create(AOwnerDocument: TCnXMLDocument; ANodeType: TCnXMLNodeType);
begin
  inherited Create;
  FOwnerDocument := AOwnerDocument;
  FNodeType := ANodeType;
  FNodeName := '';
  FNodeValue := '';
  FParentNode := nil;
  FChildNodes := TList.Create;
end;

destructor TCnXMLNode.Destroy;
var
  I: Integer;
begin
  // Free all child nodes
  if FChildNodes <> nil then
  begin
    for I := 0 to FChildNodes.Count - 1 do
      TCnXMLNode(FChildNodes[I]).Free;
    FChildNodes.Free;
  end;
  inherited;
end;

function TCnXMLNode.GetFirstChild: TCnXMLNode;
begin
  if (FChildNodes <> nil) and (FChildNodes.Count > 0) then
    Result := TCnXMLNode(FChildNodes[0])
  else
    Result := nil;
end;

function TCnXMLNode.GetLastChild: TCnXMLNode;
begin
  if (FChildNodes <> nil) and (FChildNodes.Count > 0) then
    Result := TCnXMLNode(FChildNodes[FChildNodes.Count - 1])
  else
    Result := nil;
end;

function TCnXMLNode.GetNextSibling: TCnXMLNode;
var
  Index: Integer;
begin
  Result := nil;
  if FParentNode = nil then
    Exit;

  Index := FParentNode.IndexOfChild(Self);
  if (Index >= 0) and (Index < FParentNode.ChildCount - 1) then
    Result := FParentNode.Children[Index + 1];
end;

function TCnXMLNode.GetPreviousSibling: TCnXMLNode;
var
  Index: Integer;
begin
  Result := nil;
  if FParentNode = nil then
    Exit;

  Index := FParentNode.IndexOfChild(Self);
  if Index > 0 then
    Result := FParentNode.Children[Index - 1];
end;

function TCnXMLNode.GetChildCount: Integer;
begin
  if FChildNodes <> nil then
    Result := FChildNodes.Count
  else
    Result := 0;
end;

function TCnXMLNode.GetChild(Index: Integer): TCnXMLNode;
begin
  if (FChildNodes <> nil) and (Index >= 0) and (Index < FChildNodes.Count) then
    Result := TCnXMLNode(FChildNodes[Index])
  else
    Result := nil;
end;

function TCnXMLNode.GetText: string;
var
  I: Integer;
  Child: TCnXMLNode;
begin
  Result := '';

  // For text nodes, return node value directly
  if FNodeType = xntText then
  begin
    Result := FNodeValue;
    Exit;
  end;

  // For CDATA nodes, return node value directly
  if FNodeType = xntCData then
  begin
    Result := FNodeValue;
    Exit;
  end;

  // For element nodes, concatenate all text and CDATA child nodes
  if FNodeType = xntElement then
  begin
    for I := 0 to ChildCount - 1 do
    begin
      Child := Children[I];
      if (Child.NodeType = xntText) or (Child.NodeType = xntCData) then
        Result := Result + Child.NodeValue;
    end;
  end;
end;

procedure TCnXMLNode.SetText(const Value: string);
var
  I: Integer;
  Child: TCnXMLNode;
  TextNode: TCnXMLNode;
  Found: Boolean;
begin
  // For text nodes, set node value directly
  if FNodeType = xntText then
  begin
    FNodeValue := Value;
    Exit;
  end;

  // For element nodes, find or create text child node
  if FNodeType = xntElement then
  begin
    Found := False;

    // Find existing text node
    for I := 0 to ChildCount - 1 do
    begin
      Child := Children[I];
      if Child.NodeType = xntText then
      begin
        Child.NodeValue := Value;
        Found := True;
        Break;
      end;
    end;

    // Create new text node if not found
    if not Found then
    begin
      TextNode := TCnXMLNode.Create(FOwnerDocument, xntText);
      TextNode.NodeValue := Value;
      AppendChild(TextNode);
    end;
  end;
end;

procedure TCnXMLNode.AddChild(Node: TCnXMLNode);
begin
  if FChildNodes = nil then
    FChildNodes := TList.Create;

  FChildNodes.Add(Node);
  Node.FParentNode := Self;
end;

procedure TCnXMLNode.InternalRemoveChild(Node: TCnXMLNode);
begin
  if FChildNodes <> nil then
  begin
    FChildNodes.Remove(Node);
    Node.FParentNode := nil;
  end;
end;

function TCnXMLNode.IndexOfChild(Node: TCnXMLNode): Integer;
begin
  if FChildNodes <> nil then
    Result := FChildNodes.IndexOf(Node)
  else
    Result := -1;
end;

function TCnXMLNode.AppendChild(NewChild: TCnXMLNode): TCnXMLNode;
begin
  if NewChild = nil then
  begin
    Result := nil;
    Exit;
  end;

  // Remove from old parent if exists
  if NewChild.ParentNode <> nil then
    NewChild.ParentNode.InternalRemoveChild(NewChild);

  // Add to this node
  AddChild(NewChild);
  Result := NewChild;
end;

function TCnXMLNode.InsertBefore(NewChild, RefChild: TCnXMLNode): TCnXMLNode;
var
  Index: Integer;
begin
  if NewChild = nil then
  begin
    Result := nil;
    Exit;
  end;

  // If RefChild is nil, append to end
  if RefChild = nil then
  begin
    Result := AppendChild(NewChild);
    Exit;
  end;

  // Find reference child index
  Index := IndexOfChild(RefChild);
  if Index < 0 then
  begin
    // RefChild not found, append to end
    Result := AppendChild(NewChild);
    Exit;
  end;

  // Remove from old parent if exists
  if NewChild.ParentNode <> nil then
    NewChild.ParentNode.InternalRemoveChild(NewChild);

  // Insert at index
  if FChildNodes = nil then
    FChildNodes := TList.Create;

  FChildNodes.Insert(Index, NewChild);
  NewChild.FParentNode := Self;
  Result := NewChild;
end;

function TCnXMLNode.RemoveChild(OldChild: TCnXMLNode): TCnXMLNode;
begin
  if OldChild = nil then
  begin
    Result := nil;
    Exit;
  end;

  // Check if child belongs to this node
  if OldChild.ParentNode <> Self then
  begin
    Result := nil;
    Exit;
  end;

  // Remove child
  InternalRemoveChild(OldChild);
  Result := OldChild;
end;

function TCnXMLNode.ReplaceChild(NewChild, OldChild: TCnXMLNode): TCnXMLNode;
var
  Index: Integer;
begin
  if (NewChild = nil) or (OldChild = nil) then
  begin
    Result := nil;
    Exit;
  end;

  // Find old child index
  Index := IndexOfChild(OldChild);
  if Index < 0 then
  begin
    Result := nil;
    Exit;
  end;

  // Remove from old parent if exists
  if NewChild.ParentNode <> nil then
    NewChild.ParentNode.InternalRemoveChild(NewChild);

  // Replace at index
  FChildNodes[Index] := NewChild;
  NewChild.FParentNode := Self;
  OldChild.FParentNode := nil;

  Result := OldChild;
end;

function TCnXMLNode.CloneNode(Deep: Boolean): TCnXMLNode;
var
  I: Integer;
  ChildClone: TCnXMLNode;
  SrcAttr: TCnXMLAttribute;
begin
  // Create new node with correct type
  if Self is TCnXMLDocument then
  begin
    // For document nodes, create TCnXMLDocument
    Result := TCnXMLDocument.Create;
    TCnXMLDocument(Result).Encoding := TCnXMLDocument(Self).Encoding;
    TCnXMLDocument(Result).Version := TCnXMLDocument(Self).Version;
    TCnXMLDocument(Result).Standalone := TCnXMLDocument(Self).Standalone;
  end
  else if Self is TCnXMLElement then
  begin
    // For element nodes, create TCnXMLElement to ensure FAttributes is initialized
    Result := TCnXMLElement.Create(FOwnerDocument, FNodeName);

    // Clone attributes
    for I := 0 to TCnXMLElement(Self).AttributeCount - 1 do
    begin
      SrcAttr := TCnXMLElement(Self).GetAttributeNode(TCnXMLElement(Self).AttributeNames[I]);
      if Assigned(SrcAttr) then
        TCnXMLElement(Result).SetAttribute(SrcAttr.Name, SrcAttr.Value);
    end;
  end
  else
  begin
    // For other node types, create base TCnXMLNode
    Result := TCnXMLNode.Create(FOwnerDocument, FNodeType);
    Result.NodeName := FNodeName;
    Result.NodeValue := FNodeValue;
  end;

  // Clone child nodes if deep copy
  if Deep then
  begin
    for I := 0 to ChildCount - 1 do
    begin
      ChildClone := Children[I].CloneNode(True);
      Result.AppendChild(ChildClone);
    end;
  end;
end;

function TCnXMLNode.HasChildNodes: Boolean;
begin
  Result := (FChildNodes <> nil) and (FChildNodes.Count > 0);
end;

//==============================================================================
// TCnXMLAttribute Implementation
//==============================================================================

constructor TCnXMLAttribute.Create(AOwnerElement: TCnXMLElement; const AName, AValue: string);
begin
  inherited Create;
  FOwnerElement := AOwnerElement;
  FName := AName;
  FValue := AValue;
end;

//==============================================================================
// TCnXMLElement Implementation
//==============================================================================

constructor TCnXMLElement.Create(AOwnerDocument: TCnXMLDocument; const ATagName: string);
begin
  inherited Create(AOwnerDocument, xntElement);
  FNodeName := ATagName;
  FAttributes := TList.Create;
end;

destructor TCnXMLElement.Destroy;
var
  I: Integer;
begin
  // Free all attributes
  if FAttributes <> nil then
  begin
    for I := 0 to FAttributes.Count - 1 do
      TCnXMLAttribute(FAttributes[I]).Free;
    FAttributes.Free;
  end;
  inherited;
end;

function TCnXMLElement.GetAttributeCount: Integer;
begin
  if FAttributes <> nil then
    Result := FAttributes.Count
  else
    Result := 0;
end;

function TCnXMLElement.GetAttributeName(Index: Integer): string;
begin
  if (FAttributes <> nil) and (Index >= 0) and (Index < FAttributes.Count) then
    Result := TCnXMLAttribute(FAttributes[Index]).Name
  else
    Result := '';
end;

function TCnXMLElement.GetAttributeValue(Index: Integer): string;
begin
  if (FAttributes <> nil) and (Index >= 0) and (Index < FAttributes.Count) then
    Result := TCnXMLAttribute(FAttributes[Index]).Value
  else
    Result := '';
end;

function TCnXMLElement.GetAttribute(const Name: string): string;
var
  I: Integer;
  Attr: TCnXMLAttribute;
begin
  Result := '';
  if FAttributes = nil then
    Exit;

  for I := 0 to FAttributes.Count - 1 do
  begin
    Attr := TCnXMLAttribute(FAttributes[I]);
    if Attr.Name = Name then
    begin
      Result := Attr.Value;
      Break;
    end;
  end;
end;

procedure TCnXMLElement.SetAttribute(const Name, Value: string);
var
  I: Integer;
  Attr: TCnXMLAttribute;
  Found: Boolean;
begin
  if FAttributes = nil then
    FAttributes := TList.Create;

  Found := False;

  // Find existing attribute
  for I := 0 to FAttributes.Count - 1 do
  begin
    Attr := TCnXMLAttribute(FAttributes[I]);
    if Attr.Name = Name then
    begin
      Attr.Value := Value;
      Found := True;
      Break;
    end;
  end;

  // Create new attribute if not found
  if not Found then
  begin
    Attr := TCnXMLAttribute.Create(Self, Name, Value);
    FAttributes.Add(Attr);
  end;
end;

function TCnXMLElement.HasAttribute(const Name: string): Boolean;
var
  I: Integer;
  Attr: TCnXMLAttribute;
begin
  Result := False;
  if FAttributes = nil then
    Exit;

  for I := 0 to FAttributes.Count - 1 do
  begin
    Attr := TCnXMLAttribute(FAttributes[I]);
    if Attr.Name = Name then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TCnXMLElement.RemoveAttribute(const Name: string);
var
  I: Integer;
  Attr: TCnXMLAttribute;
begin
  if FAttributes = nil then
    Exit;

  for I := 0 to FAttributes.Count - 1 do
  begin
    Attr := TCnXMLAttribute(FAttributes[I]);
    if Attr.Name = Name then
    begin
      FAttributes.Delete(I);
      Attr.Free;
      Break;
    end;
  end;
end;

function TCnXMLElement.GetAttributeNode(const Name: string): TCnXMLAttribute;
var
  I: Integer;
  Attr: TCnXMLAttribute;
begin
  Result := nil;
  if FAttributes = nil then
    Exit;

  for I := 0 to FAttributes.Count - 1 do
  begin
    Attr := TCnXMLAttribute(FAttributes[I]);
    if Attr.Name = Name then
    begin
      Result := Attr;
      Break;
    end;
  end;
end;

function TCnXMLElement.GetElementsByTagName(const TagName: string): TList;
var
  I: Integer;
  Child: TCnXMLNode;

  procedure CollectElements(Node: TCnXMLNode);
  var
    J: Integer;
    ChildNode: TCnXMLNode;
  begin
    if Node = nil then
      Exit;

    // Check if this node matches
    if (Node.NodeType = xntElement) and (Node.NodeName = TagName) then
      Result.Add(Node);

    // Recursively check child nodes
    for J := 0 to Node.ChildCount - 1 do
    begin
      ChildNode := Node.Children[J];
      CollectElements(ChildNode);
    end;
  end;

begin
  Result := TList.Create;

  // Collect matching elements from child nodes
  for I := 0 to ChildCount - 1 do
  begin
    Child := Children[I];
    CollectElements(Child);
  end;
end;

//==============================================================================
// TCnXMLDocument Implementation
//==============================================================================

constructor TCnXMLDocument.Create;
begin
  inherited Create(nil, xntDocument);
  FOwnerDocument := Self;  // Document owns itself
  FDocumentElement := nil;
  FEncoding := 'UTF-8';
  FVersion := '1.0';
  FStandalone := '';
  FPreserveWhitespace := False;
end;

destructor TCnXMLDocument.Destroy;
begin
  // Document element will be freed by inherited destructor
  inherited;
end;

function TCnXMLDocument.CreateElement(const TagName: string): TCnXMLElement;
begin
  Result := TCnXMLElement.Create(Self, TagName);
end;

function TCnXMLDocument.CreateTextNode(const Data: string): TCnXMLNode;
begin
  Result := TCnXMLNode.Create(Self, xntText);
  Result.NodeValue := Data;
end;

function TCnXMLDocument.CreateComment(const Data: string): TCnXMLNode;
begin
  Result := TCnXMLNode.Create(Self, xntComment);
  Result.NodeValue := Data;
end;

function TCnXMLDocument.CreateCDATASection(const Data: string): TCnXMLNode;
begin
  Result := TCnXMLNode.Create(Self, xntCData);
  Result.NodeValue := Data;
end;

function TCnXMLDocument.CreateProcessingInstruction(const Target, Data: string): TCnXMLNode;
begin
  Result := TCnXMLNode.Create(Self, xntPI);
  Result.NodeName := Target;
  Result.NodeValue := Data;
end;

function TCnXMLDocument.AppendChild(NewChild: TCnXMLNode): TCnXMLNode;
begin
  // Call inherited to add child
  Result := inherited AppendChild(NewChild);

  // Auto-set DocumentElement if it's an element node and DocumentElement is not set
  if (FDocumentElement = nil) and (NewChild is TCnXMLElement) then
    FDocumentElement := TCnXMLElement(NewChild);
end;

procedure TCnXMLDocument.LoadFromString(const XMLString: string);
var
  Parser: TCnXMLParser;
  TempDoc: TCnXMLDocument;
  I: Integer;
begin
  // Clear current content
  for I := ChildCount - 1 downto 0 do
  begin
    Children[I].Free;
    FChildNodes.Delete(I);
  end;
  FDocumentElement := nil;

  // Parse XML string
  Parser := TCnXMLParser.Create(XMLString);
  try
    TempDoc := Parser.Parse;
    try
      // Copy properties
      FVersion := TempDoc.Version;
      FEncoding := TempDoc.Encoding;
      FStandalone := TempDoc.Standalone;

      // Move document element
      if TempDoc.DocumentElement <> nil then
      begin
        TempDoc.InternalRemoveChild(TempDoc.DocumentElement);
        TempDoc.DocumentElement.FOwnerDocument := Self;
        AppendChild(TempDoc.DocumentElement);
        FDocumentElement := TempDoc.DocumentElement;
      end;
    finally
      TempDoc.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TCnXMLDocument.LoadFromStream(Stream: TStream);
var
  XMLString: string;
  Buffer: TBytes;
  StartPos: Integer;
  DataSize: Integer;
  IsUtf16LE: Boolean;
  IsUtf16BE: Boolean;
  I: Integer;
  Temp: Byte;
{$IFDEF UNICODE}
  StringStream: TStringStream;
{$ELSE}
  UTF8Str: AnsiString;
  WideText: WideString;
{$ENDIF}
begin
  DataSize := Stream.Size;
  if DataSize > 0 then
  begin
    SetLength(Buffer, DataSize);
    Stream.Position := 0;
    Stream.ReadBuffer(Buffer[0], DataSize);
  end
  else
    SetLength(Buffer, 0);

  StartPos := 0;
  IsUtf16LE := False;
  IsUtf16BE := False;
  // Detect and skip BOM based on data content
  if (Length(Buffer) >= 3) and
     (Buffer[0] = $EF) and (Buffer[1] = $BB) and (Buffer[2] = $BF) then
    StartPos := 3
  else if (Length(Buffer) >= 2) and
    (Buffer[0] = $FF) and (Buffer[1] = $FE) then
  begin
    StartPos := 2;
    IsUtf16LE := True;
  end
  else if (Length(Buffer) >= 2) and
    (Buffer[0] = $FE) and (Buffer[1] = $FF) then
  begin
    StartPos := 2;
    IsUtf16BE := True;
  end;

{$IFDEF UNICODE}
  if IsUtf16LE or IsUtf16BE then
  begin
    DataSize := Length(Buffer) - StartPos;
    if (DataSize and 1) <> 0 then
      Dec(DataSize);
    if DataSize > 0 then
    begin
      if IsUtf16BE then
      begin
        for I := 0 to (DataSize div 2) - 1 do
        begin
          Temp := Buffer[StartPos + I * 2];
          Buffer[StartPos + I * 2] := Buffer[StartPos + I * 2 + 1];
          Buffer[StartPos + I * 2 + 1] := Temp;
        end;
      end;
      SetLength(XMLString, DataSize div 2);
      Move(Buffer[StartPos], XMLString[1], DataSize);
    end
    else
      XMLString := '';
    LoadFromString(XMLString);
  end
  else
  begin
    StringStream := TStringStream.Create('', TEncoding.UTF8);
    try
      if Length(Buffer) > StartPos then
        StringStream.WriteBuffer(Buffer[StartPos], Length(Buffer) - StartPos);
      XMLString := StringStream.DataString;
      LoadFromString(XMLString);
    finally
      StringStream.Free;
    end;
  end;
{$ELSE}
  if IsUtf16LE or IsUtf16BE then
  begin
    DataSize := Length(Buffer) - StartPos;
    if (DataSize and 1) <> 0 then
      Dec(DataSize);
    if DataSize > 0 then
    begin
      if IsUtf16BE then
      begin
        for I := 0 to (DataSize div 2) - 1 do
        begin
          Temp := Buffer[StartPos + I * 2];
          Buffer[StartPos + I * 2] := Buffer[StartPos + I * 2 + 1];
          Buffer[StartPos + I * 2 + 1] := Temp;
        end;
      end;
      SetLength(WideText, DataSize div 2);
      Move(Buffer[StartPos], WideText[1], DataSize);
    end
    else
      WideText := '';
    XMLString := AnsiString(WideText);
    LoadFromString(XMLString);
  end
  else
  begin
    // 非 Unicode Delphi：从 UTF-8 转换为 AnsiString
    if Length(Buffer) > StartPos then
    begin
      SetLength(UTF8Str, Length(Buffer) - StartPos);
      Move(Buffer[StartPos], UTF8Str[1], Length(UTF8Str));
    end
    else
      UTF8Str := '';

    XMLString := CnUtf8ToAnsi(UTF8Str);
    LoadFromString(XMLString);
  end;
{$ENDIF}
end;

procedure TCnXMLDocument.LoadFromFile(const FileName: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

function TCnXMLDocument.SaveToString(Indent: Boolean): string;

  function EscapeText(const Text: string): string;
  var
    I: Integer;
    Ch: Char;
  begin
    Result := '';
    for I := 1 to Length(Text) do
    begin
      Ch := Text[I];
      // Only escape ASCII special characters
      if Ch = '<' then
        Result := Result + '&lt;'
      else if Ch = '>' then
        Result := Result + '&gt;'
      else if Ch = '&' then
        Result := Result + '&amp;'
      else if Ch = '"' then
        Result := Result + '&quot;'
      else if Ch = '''' then
        Result := Result + '&apos;'
      else
        Result := Result + Ch;
    end;
  end;

  function GetIndentString(Level: Integer): string;
  var
    I: Integer;
  begin
    Result := '';
    if Indent then
      for I := 1 to Level * 2 do
        Result := Result + ' ';
  end;

  procedure SerializeNode(Node: TCnXMLNode; Level: Integer; var Output: string);
  var
    I: Integer;
    Element: TCnXMLElement;
    AttrStr: string;
    HasOnlyText: Boolean;
  begin
    case Node.NodeType of
      xntElement:
        begin
          Element := TCnXMLElement(Node);

          // Start tag
          Output := Output + GetIndentString(Level) + '<' + Element.TagName;

          // Attributes
          for I := 0 to Element.AttributeCount - 1 do
          begin
            AttrStr := Element.AttributeNames[I];
            Output := Output + ' ' + AttrStr + '="' +
                     EscapeText(Element.AttributeValues[I]) + '"';
          end;

          // Check if has children
          if Element.HasChildNodes then
          begin
            Output := Output + '>';

            // Check if element contains only text content (no child elements)
            HasOnlyText := True;
            for I := 0 to Element.ChildCount - 1 do
            begin
              if Element.Children[I].NodeType = xntElement then
              begin
                HasOnlyText := False;
                Break;
              end;
            end;

            // Add newline after start tag only if element has child elements
            if Indent and not HasOnlyText then
              Output := Output + #13#10;

            // Serialize children
            for I := 0 to Element.ChildCount - 1 do
              SerializeNode(Element.Children[I], Level + 1, Output);

            // End tag
            if Indent and not HasOnlyText then
              Output := Output + GetIndentString(Level);
            Output := Output + '</' + Element.TagName + '>';
          end
          else
          begin
            // Self-closing tag
            Output := Output + '/>';
          end;

          if Indent then
            Output := Output + #13#10;
        end;

      xntText:
        begin
          // Only output non-empty text nodes
          // Don't add indentation or newlines for text content
          // to preserve the original text formatting
          if Trim(Node.NodeValue) <> '' then
            Output := Output + EscapeText(Node.NodeValue);
        end;

      xntCData:
        begin
          Output := Output + GetIndentString(Level) + '<![CDATA[' +
                   Node.NodeValue + ']]>';
          if Indent then
            Output := Output + #13#10;
        end;

      xntComment:
        begin
          Output := Output + GetIndentString(Level) + '<!--' +
                   Node.NodeValue + '-->';
          if Indent then
            Output := Output + #13#10;
        end;

      xntPI:
        begin
          Output := Output + GetIndentString(Level) + '<?' +
                   Node.NodeName + ' ' + Node.NodeValue + '?>';
          if Indent then
            Output := Output + #13#10;
        end;
    end;
  end;

begin
  Result := '';

  // XML declaration
  if FVersion <> '' then
  begin
    Result := '<?xml version="' + FVersion + '"';
    if FEncoding <> '' then
      Result := Result + ' encoding="' + FEncoding + '"';
    if FStandalone <> '' then
      Result := Result + ' standalone="' + FStandalone + '"';
    Result := Result + '?>';
    if Indent then
      Result := Result + #13#10;
  end;

  // Serialize document element
  if FDocumentElement <> nil then
    SerializeNode(FDocumentElement, 0, Result);
end;

procedure TCnXMLDocument.SaveToStream(Stream: TStream; Indent: Boolean);
var
  XMLString: string;
{$IFDEF UNICODE}
  StringStream: TStringStream;
{$ELSE}
  UTF8Str: AnsiString;
{$ENDIF}
begin
  XMLString := SaveToString(Indent);
{$IFDEF UNICODE}
  StringStream := TStringStream.Create(XMLString, TEncoding.UTF8);
  try
    Stream.CopyFrom(StringStream, 0);
  finally
    StringStream.Free;
  end;
{$ELSE}
  // 非 Unicode Delphi：将 AnsiString 转换为 UTF-8
  UTF8Str := CnAnsiToUtf8(XMLString);
  if Length(UTF8Str) > 0 then
    Stream.Write(UTF8Str[1], Length(UTF8Str));
{$ENDIF}
end;

procedure TCnXMLDocument.SaveToFile(const FileName: string; Indent: Boolean);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FileStream, Indent);
  finally
    FileStream.Free;
  end;
end;

//==============================================================================
// TCnXMLParser Implementation
//==============================================================================

constructor TCnXMLParser.Create(const ASource: string);
begin
  inherited Create;
  FLexer := TCnXMLLexer.Create(ASource);
  FDocument := nil;
  FCurrentToken.TokenType := xttNone;
end;

destructor TCnXMLParser.Destroy;
begin
  FLexer.Free;
  inherited;
end;

procedure TCnXMLParser.NextToken;
begin
  FCurrentToken := FLexer.NextToken;
end;

procedure TCnXMLParser.RaiseError(const Msg: string);
begin
  raise ECnXMLException.Create(Msg, CN_XML_ERR_INVALID_STRUCTURE,
    FCurrentToken.Line, FCurrentToken.Column);
end;

procedure TCnXMLParser.ParseXMLDecl;
var
  I: Integer;
  AttrStr, AttrName, AttrValue: string;
  EqualPos: Integer;
begin
  // XML declaration is optional
  if FCurrentToken.TokenType <> xttXMLDecl then
    Exit;

  // Parse attributes from XML declaration
  if FCurrentToken.Attributes <> nil then
  begin
    for I := 0 to FCurrentToken.Attributes.Count - 1 do
    begin
      AttrStr := FCurrentToken.Attributes[I];
      EqualPos := Pos('=', AttrStr);
      if EqualPos > 0 then
      begin
        AttrName := Copy(AttrStr, 1, EqualPos - 1);
        AttrValue := Copy(AttrStr, EqualPos + 1, Length(AttrStr) - EqualPos);

        if AttrName = 'version' then
          FDocument.Version := AttrValue
        else if AttrName = 'encoding' then
          FDocument.Encoding := AttrValue
        else if AttrName = 'standalone' then
          FDocument.Standalone := AttrValue;
      end;
    end;
  end;

  NextToken;  // Move to next token
end;

procedure TCnXMLParser.ParseElement(ParentNode: TCnXMLNode);
var
  Element: TCnXMLElement;
  TagName: string;
  I: Integer;
  AttrStr, AttrName, AttrValue: string;
  EqualPos: Integer;
begin
  // Expect start tag or empty tag
  if not (FCurrentToken.TokenType in [xttStartTag, xttEmptyTag]) then
    RaiseError('Expected start tag or empty tag');

  TagName := FCurrentToken.Value;

  // Create element node
  Element := FDocument.CreateElement(TagName);
  ParentNode.AppendChild(Element);

  // Parse attributes
  if FCurrentToken.Attributes <> nil then
  begin
    for I := 0 to FCurrentToken.Attributes.Count - 1 do
    begin
      AttrStr := FCurrentToken.Attributes[I];
      EqualPos := Pos('=', AttrStr);
      if EqualPos > 0 then
      begin
        AttrName := Copy(AttrStr, 1, EqualPos - 1);
        AttrValue := Copy(AttrStr, EqualPos + 1, Length(AttrStr) - EqualPos);
        Element.SetAttribute(AttrName, AttrValue);
      end;
    end;
  end;

  // If empty tag, we're done
  if FCurrentToken.TokenType = xttEmptyTag then
  begin
    NextToken;
    Exit;
  end;

  // Parse content
  NextToken;
  ParseContent(Element);

  // Expect end tag
  if FCurrentToken.TokenType <> xttEndTag then
    RaiseError('Expected end tag');

  if FCurrentToken.Value <> TagName then
    raise ECnXMLException.Create('Tag mismatch: expected </' + TagName + '> but got </' +
                                 FCurrentToken.Value + '>',
                                 CN_XML_ERR_TAG_MISMATCH,
                                 FCurrentToken.Line, FCurrentToken.Column);

  NextToken;
end;

procedure TCnXMLParser.ParseContent(ParentNode: TCnXMLNode);
var
  Node: TCnXMLNode;
begin
  while FCurrentToken.TokenType <> xttEOF do
  begin
    case FCurrentToken.TokenType of
      xttStartTag, xttEmptyTag:
        ParseElement(ParentNode);

      xttText:
        begin
          if Trim(FCurrentToken.Value) <> '' then
          begin
            Node := FDocument.CreateTextNode(FCurrentToken.Value);
            ParentNode.AppendChild(Node);
          end;
          NextToken;
        end;

      xttCData:
        begin
          Node := FDocument.CreateCDATASection(FCurrentToken.Value);
          ParentNode.AppendChild(Node);
          NextToken;
        end;

      xttComment:
        begin
          Node := FDocument.CreateComment(FCurrentToken.Value);
          ParentNode.AppendChild(Node);
          NextToken;
        end;

      xttPI:
        begin
          Node := FDocument.CreateProcessingInstruction('', FCurrentToken.Value);
          ParentNode.AppendChild(Node);
          NextToken;
        end;

      xttEndTag:
        Break;  // End of content

    else
      RaiseError('Unexpected token in content');
    end;
  end;
end;

function TCnXMLParser.Parse: TCnXMLDocument;
begin
  FDocument := TCnXMLDocument.Create;
  try
    NextToken;  // Get first token

    // Parse optional XML declaration
    ParseXMLDecl;

    // Skip comments and other non-element tokens before root element
    while FCurrentToken.TokenType in [xttComment, xttPI] do
      NextToken;

    // Parse root element
    if FCurrentToken.TokenType in [xttStartTag, xttEmptyTag] then
    begin
      ParseElement(FDocument);
      FDocument.DocumentElement := TCnXMLElement(FDocument.FirstChild);
    end
    else
      RaiseError('Expected root element');

    Result := FDocument;
  except
    FDocument.Free;
    raise;
  end;
end;

//==============================================================================
// Encoding Functions Implementation
//==============================================================================

function CnXMLDetectEncoding(const Buffer: TBytes): Integer;
begin
  Result := CN_XML_ENCODING_UNKNOWN;

  if Length(Buffer) < 2 then
    Exit;

  // Check BOM (Byte Order Mark)
  // UTF-8 BOM: EF BB BF
  if (Length(Buffer) >= 3) and
     (Buffer[0] = $EF) and (Buffer[1] = $BB) and (Buffer[2] = $BF) then
  begin
    Result := CN_XML_ENCODING_UTF8;
    Exit;
  end;

  // UTF-16 LE BOM: FF FE
  if (Buffer[0] = $FF) and (Buffer[1] = $FE) then
  begin
    Result := CN_XML_ENCODING_UTF16LE;
    Exit;
  end;

  // UTF-16 BE BOM: FE FF
  if (Buffer[0] = $FE) and (Buffer[1] = $FF) then
  begin
    Result := CN_XML_ENCODING_UTF16BE;
    Exit;
  end;

  // Default to GBK (project standard)
  Result := CN_XML_ENCODING_GBK;
end;

function CnXMLConvertEncoding(const Source: AnsiString;
  SourceEncoding, TargetEncoding: Integer): AnsiString;
begin
  // For now, simple implementation - just return source
  // Full implementation would use Windows API or iconv
  // This is a placeholder for basic functionality
  Result := Source;

  // TODO: Implement proper encoding conversion
  // - Use MultiByteToWideChar and WideCharToMultiByte on Windows
  // - Use iconv or similar on Unix/Linux
  // - Handle GBK, UTF-8, UTF-16 conversions
end;

//==============================================================================
// Utility Functions Implementation
//==============================================================================

function CnXMLCreateDocument: TCnXMLDocument;
begin
  Result := TCnXMLDocument.Create;
end;

//==============================================================================
// String Conversion Helper Functions Implementation
//==============================================================================

function CnXMLStrToRealDef(const S: string; Default: Extended): Extended;
begin
  try
    Result := StrToFloat(StringReplace(S, '.', {$IFDEF DELPHIXE3_UP}FormatSettings.{$ENDIF}DecimalSeparator, [rfReplaceAll]));
  except
    on EConvertError do
      Result := Default;
  end;
end;

function Str2Time(S: string): TDateTime;
const
  TIME_SEP = ':';
var
  H: Word;
  M: Word;
  SC: Word;
  MS: Word;
  P: Integer;
begin
  S := Trim(S);
  if S = '' then
    Result := 0
  else
  begin
    P := Pos(TIME_SEP, S);
    H := StrToInt(Copy(S, 1, P - 1));
    Delete(S, 1, P);
    P := Pos(TIME_SEP, S);
    M := StrToInt(Copy(S, 1, P - 1));
    Delete(S, 1, P);
    P := Pos('.', S);
    if P > 0 then
    begin
      MS := StrToInt(Copy(S, P + 1, Length(S) - P));
      Delete(S, P, Length(S) - P + 1);
    end
    else
      MS := 0;

    SC := StrToInt(S);
    Result := EncodeTime(H, M, SC, MS);
  end;
end;

function ISODateTime2DateTime (const IsoDT: String): TDateTime;
const
  DATESEP = '-';
var
  Day: Word;
  Month: Word;
  Year: Word;
  P: Integer;
  sDate: string;
  sTime: string;
begin
  P := Pos('T', IsoDT);
  if (P = 0) and (Pos('-', IsoDT) > 0) then
    P := Length(IsoDT) + 1;

  sDate := Trim(Copy(IsoDT, 1, P - 1));
  sTime := Trim(Copy(IsoDT, P + 1, Length(IsoDT) - P));
  Result := 0;

  if sDate <> '' then
  begin
    P := Pos (DATESEP, sDate);
    Year :=  StrToInt(Copy(sDate, 1, P - 1));
    Delete(sDate, 1, P);
    P := Pos(DATESEP, sDate);
    Month := StrToInt(Copy(sDate, 1, P - 1));
    Day := StrToInt(Copy(sDate, P + 1, Length(sDate) - P));
    Result := EncodeDate(Year, Month, Day);
  end;
  Result := Result + Frac(Str2Time(sTime));
end;

function CnXMLStrToDateTime(const S: string; var Value: TDateTime): Boolean;
begin
  try
    Value := ISODateTime2DateTime(S);
    Result := True;
  except
    Result := False;
  end;
end;

function CnXMLStrToInt(const S: string; var Value: Integer): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;

function CnXMLStrToIntDef(const S: string; Default: Integer): Integer;
begin
  if not CnXMLStrToInt(S, Result) then
    Result := Default;
end;

function CnXMLStrToBool(const S: string; var Value: Boolean): Boolean;
begin
  // 支持 0/1 和 True/False 格式
  if (S = '1') or (CompareText(S, 'True') = 0) then
  begin
    Value := True;
    Result := True;
  end
  else if (S = '0') or (CompareText(S, 'False') = 0) then
  begin
    Value := False;
    Result := True;
  end
  else
    Result := False;
end;

function CnXMLStrToInt64(const S: string; var Value: Int64): Boolean;
begin
  try
    Value := StrToInt64(S);
    Result := True;
  except
    on EConvertError do
      Result := False;
  end;
end;

function CnXMLIntToStr(Value: Integer): string;
begin
  Result := IntToStr(Value);
end;

function CnXMLBoolToStr(Value: Boolean): string;
begin
  if Value then
    Result := 'True'
  else
    Result := 'False';
end;

function CnXMLRealToStr(Value: Extended): string;
begin
  Result := StringReplace(FloatToStr(Value), {$IFDEF DELPHIXE3_UP}FormatSettings.{$ENDIF}DecimalSeparator, '.', [rfReplaceAll]);
end;

function CnXMLInt64ToStr(Value: Int64): string;
begin
  Result := IntToStr(Value);
end;

function CnXMLDateTimeToStrEx(Value: TDateTime): string;
begin
  // 返回 ISO 格式的日期时间字符串
  if Trunc(Value) = 0 then
    Result := FormatDateTime('hh:nn:ss', Value)
  else if Frac(Value) = 0 then
    Result := FormatDateTime('yyyy-mm-dd', Value)
  else
    Result := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', Value);
end;

//==============================================================================
// TCnXMLWriter Implementation
//==============================================================================

constructor TCnXMLWriter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDocument := nil;
  FRootNode := nil;
  FEncoding := 'UTF-8';
  FOwnsDocument := True;
  FUseDataNode := True;  // 默认使用 <data> 节点以保持兼容性
  InitDocument;
end;

destructor TCnXMLWriter.Destroy;
begin
  if FOwnsDocument and (FDocument <> nil) then
    FDocument.Free;
  inherited;
end;

procedure TCnXMLWriter.SetUseDataNode(const Value: Boolean);
begin
  if FUseDataNode <> Value then
  begin
    FUseDataNode := Value;
    InitDocument;  // 重新初始化文档以匹配新的设置
  end;
end;

procedure TCnXMLWriter.InitDocument;
begin
  if FOwnsDocument and (FDocument <> nil) then
    FDocument.Free;

  FDocument := TCnXMLDocument.Create;
  FOwnsDocument := True;
  FDocument.Encoding := FEncoding;
  FDocument.Version := '1.0';

  if FUseDataNode then
  begin
    // Create root node - use 'data' for compatibility
    FRootNode := FDocument.CreateElement('data');
    FRootNode.SetAttribute('PropFormat', 'node');
    FDocument.AppendChild(FRootNode);
  end
  else
  begin
    // No wrapper node - object node will be added directly to document
    FRootNode := nil;
  end;
end;

function TCnXMLWriter.GetXMLString: string;
begin
  if FDocument <> nil then
    Result := FDocument.SaveToString(False)
  else
    Result := '';
end;

procedure TCnXMLWriter.SetXMLString(const Value: string);
begin
  if FOwnsDocument and (FDocument <> nil) then
    FDocument.Free;

  FDocument := TCnXMLDocument.Create;
  FOwnsDocument := True;
  FDocument.LoadFromString(Value);

  // Find root node - use 'data' for compatibility
  if FDocument.DocumentElement <> nil then
    FRootNode := FDocument.DocumentElement
  else
  begin
    FRootNode := FDocument.CreateElement('data');
    FRootNode.SetAttribute('PropFormat', 'node');
    FDocument.AppendChild(FRootNode);
  end;
end;

procedure TCnXMLWriter.SetClassType(const Obj: TPersistent; Node: TCnXMLElement);
begin
  Node.SetAttribute('ClassType', Obj.ClassName);

  // Set persistent type
  if Obj is TCollection then
    Node.SetAttribute('PersistentType', 'TCollection')
  else if Obj is TCollectionItem then
    Node.SetAttribute('PersistentType', 'TCollectionItem')
  else if Obj is TComponent then
    Node.SetAttribute('PersistentType', 'TComponent')
  else
    Node.SetAttribute('PersistentType', 'TPersistent');
end;

procedure TCnXMLWriter.WriteCollection(const Collection: TCollection; Node: TCnXMLElement);
var
  I: Integer;
  Item: TCollectionItem;
  ItemNode: TCnXMLElement;
begin
  for I := 0 to Collection.Count - 1 do
  begin
    Item := Collection.Items[I];
    // Use 'o' node name for compatibility
    ItemNode := FDocument.CreateElement(CN_XML_COLLECTION_ITEM_NODE);
    Node.AppendChild(ItemNode);

    SetClassType(Item, ItemNode);
    WriteProperties(Item, ItemNode);
  end;
end;

procedure TCnXMLWriter.WriteProperties(const Obj: TPersistent; Node: TCnXMLElement);
var
  I, PropCount: Integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropObj: TObject;
  PropValue: string;
  ChildNode: TCnXMLElement;
begin
  PropCount := GetTypeData(Obj.ClassInfo)^.PropCount;
  if PropCount = 0 then
    Exit;

  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(Obj.ClassInfo, tkAny, PropList);

    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];

      case PropInfo.PropType^.Kind of
        tkClass:
          begin
            PropObj := GetObjectProp(Obj, string(PropInfo.Name));
            if PropObj <> nil then
            begin
              if PropObj is TPersistent then
              begin
                ChildNode := FDocument.CreateElement(string(PropInfo.Name));
                Node.AppendChild(ChildNode);

                SetClassType(TPersistent(PropObj), ChildNode);
                if PropObj is TCollection then
                  WriteCollection(TCollection(PropObj), ChildNode)
                else
                  WriteProperties(TPersistent(PropObj), ChildNode);
              end;
            end;
          end;

        tkEnumeration:
          begin
            PropValue := GetEnumProp(Obj, string(PropInfo.Name));
            // Store as child element for compatibility
            ChildNode := FDocument.CreateElement(string(PropInfo.Name));
            ChildNode.AppendChild(FDocument.CreateTextNode(PropValue));
            Node.AppendChild(ChildNode);
          end;

        tkSet:
          begin
            PropValue := GetSetProp(Obj, string(PropInfo.Name), True); // with []
            // Store as child element for compatibility
            ChildNode := FDocument.CreateElement(string(PropInfo.Name));
            ChildNode.AppendChild(FDocument.CreateTextNode(PropValue));
            Node.AppendChild(ChildNode);
          end;

        tkInteger, tkChar, tkWChar, tkFloat, tkString, tkLString,
        tkWString{$IFDEF UNICODE}, tkUString{$ENDIF}, tkVariant, tkInt64:
          begin
            PropValue := GetPropValue(Obj, string(PropInfo.Name));
            // Store as child element for compatibility
            ChildNode := FDocument.CreateElement(string(PropInfo.Name));
            ChildNode.AppendChild(FDocument.CreateTextNode(PropValue));
            Node.AppendChild(ChildNode);
          end;
      end;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

procedure TCnXMLWriter.WriteObjectToXML(const Obj: TPersistent; const RootName: string);
var
  ObjNode: TCnXMLElement;
  NodeList: TList;
  CName: string;
begin
  if Obj = nil then
    Exit;

  // Use class name as node name for compatibility
  if RootName <> '' then
    CName := RootName
  else
    CName := Obj.ClassName;

  // Find or create object node
  if FUseDataNode then
  begin
    if FRootNode = nil then
      raise ECnXMLException.Create('Root node not initialized', CN_XML_ERR_INVALID_OPERATION, 0, 0);

    NodeList := FRootNode.GetElementsByTagName(CName);
    try
      if NodeList.Count > 0 then
        ObjNode := TCnXMLElement(NodeList[0])
      else
        ObjNode := nil;
    finally
      NodeList.Free;
    end;

    if ObjNode = nil then
    begin
      ObjNode := FDocument.CreateElement(CName);
      FRootNode.AppendChild(ObjNode);
    end
    else
    begin
      // Clear existing content
      while ObjNode.HasChildNodes do
        ObjNode.RemoveChild(ObjNode.FirstChild);
    end;
  end
  else
  begin
    // Without data node, check if document element exists
    if (FDocument.DocumentElement <> nil) and
       (FDocument.DocumentElement.TagName = CName) then
      ObjNode := FDocument.DocumentElement
    else
      ObjNode := nil;

    if ObjNode = nil then
    begin
      ObjNode := FDocument.CreateElement(CName);
      FDocument.AppendChild(ObjNode);  // 直接调用 FDocument.AppendChild
      FRootNode := ObjNode;
    end
    else
    begin
      // Clear existing content
      while ObjNode.HasChildNodes do
        ObjNode.RemoveChild(ObjNode.FirstChild);
    end;
  end;

  if Obj is TCollection then
    WriteCollection(TCollection(Obj), ObjNode)
  else
    WriteProperties(Obj, ObjNode);
end;

procedure TCnXMLWriter.SaveToFile(const FileName: string; Indent: Boolean);
begin
  if FDocument <> nil then
    FDocument.SaveToFile(FileName, Indent);
end;

//==============================================================================
// TCnXMLReader Implementation
//==============================================================================

constructor TCnXMLReader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDocument := nil;
  FRootNode := nil;
  FEncoding := 'UTF-8';
  FOwnsDocument := True;
end;

destructor TCnXMLReader.Destroy;
begin
  if FOwnsDocument and (FDocument <> nil) then
    FDocument.Free;
  inherited;
end;

function TCnXMLReader.GetXMLString: string;
begin
  if FDocument <> nil then
    Result := FDocument.SaveToString(False)
  else
    Result := '';
end;

procedure TCnXMLReader.SetXMLString(const Value: string);
begin
  if FOwnsDocument and (FDocument <> nil) then
    FDocument.Free;

  FDocument := TCnXMLDocument.Create;
  FOwnsDocument := True;
  FDocument.LoadFromString(Value);

  FindRootNode;
end;

procedure TCnXMLReader.FindRootNode;
begin
  if FDocument = nil then
    raise ECnXMLException.Create('Document not loaded', CN_XML_ERR_INVALID_OPERATION, 0, 0);

  if FDocument.DocumentElement <> nil then
    FRootNode := FDocument.DocumentElement
  else
    raise ECnXMLException.Create('Root node not found', CN_XML_ERR_MISSING_ROOT, 0, 0);
end;

procedure TCnXMLReader.SetPropertyValue(Obj: TPersistent; const PropName, PropValue: string; PProp: PPropInfo);
var
  ConvertedValue: string;
  B: Boolean;
begin
  case PProp.PropType^.Kind of
    tkEnumeration:
      begin
        // Special handling for Boolean type or uses 0/1
{$IFDEF FPC}
        B := PProp.PropType = TypeInfo(Boolean);
{$ELSE}
        B := PProp.PropType^ = TypeInfo(Boolean);
{$ENDIF}
        if B then
        begin
          if PropValue = '0' then
            ConvertedValue := 'False'
          else if PropValue = '1' then
            ConvertedValue := 'True'
          else
            ConvertedValue := PropValue;  // Already 'True' or 'False'
          SetEnumProp(Obj, PropName, ConvertedValue);
        end
        else
          SetEnumProp(Obj, PropName, PropValue);
      end;
    tkSet:
      begin
{$IFDEF COMPILER5}
        if PropValue = '' then // Empty will cause AV under D5, using []
          SetSetProp(Obj, PropName, '[]')
        else
{$ENDIF}
          SetSetProp(Obj, PropName, PropValue);
      end;
  else
      SetPropValue(Obj, PropName, PropValue);
  end;
end;

procedure TCnXMLReader.ReadCollection(Collection: TCollection; Node: TCnXMLElement);
var
  I: Integer;
  ItemNode: TCnXMLNode;
  Item: TCollectionItem;
  ClassName: string;
begin
  Collection.Clear;

  for I := 0 to Node.ChildCount - 1 do
  begin
    ItemNode := Node.Children[I];
    // Support both 'o' and 'Item' for backward compatibility
    if (ItemNode.NodeType = xntElement) and
       ((ItemNode.NodeName = CN_XML_COLLECTION_ITEM_NODE) or
        (ItemNode.NodeName = 'Item')) then
    begin
      ClassName := TCnXMLElement(ItemNode).GetAttribute('ClassType');

      // Create item
      Item := Collection.Add;

      // Read properties
      ReadProperties(Item, TCnXMLElement(ItemNode));
    end;
  end;
end;

procedure TCnXMLReader.ReadProperties(const Obj: TPersistent; Node: TCnXMLElement);
var
  I, J, PropCount: Integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropObj: TObject;
  PropValue: string;
  ChildNode: TCnXMLNode;
begin
  PropCount := GetTypeData(Obj.ClassInfo)^.PropCount;
  if PropCount = 0 then
    Exit;

  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(Obj.ClassInfo, tkAny, PropList);

    // Read child nodes (For compatibility - properties stored as child elements)
    for I := 0 to Node.ChildCount - 1 do
    begin
      ChildNode := Node.Children[I];
      if ChildNode.NodeType <> xntElement then
        Continue;

      // Find matching property
      for J := 0 to PropCount - 1 do
      begin
        PropInfo := PropList^[J];
        if string(PropInfo.Name) = ChildNode.NodeName then
        begin
          if PropInfo.PropType^.Kind = tkClass then
          begin
            // Complex property (object)
            PropObj := GetObjectProp(Obj, string(PropInfo.Name));
            if PropObj <> nil then
            begin
              if PropObj is TCollection then
                ReadCollection(TCollection(PropObj), TCnXMLElement(ChildNode))
              else if PropObj is TPersistent then
                ReadProperties(TPersistent(PropObj), TCnXMLElement(ChildNode));
            end;
          end
          else
          begin
            // Simple property - read text content from child element
            PropValue := ChildNode.Text;
            SetPropertyValue(Obj, string(PropInfo.Name), PropValue, PropInfo);
          end;
          Break;
        end;
      end;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

function TCnXMLReader.ReadObjectFromXML(Obj: TPersistent; const RootName: string): Boolean;
var
  ObjNode: TCnXMLElement;
  NodeList: TList;
  CName: string;
begin
  Result := False;

  if (Obj = nil) or (FRootNode = nil) then
    Exit;

  // For compatibility, find node by class name
  if RootName <> '' then
    CName := RootName
  else
    CName := Obj.ClassName;

  // First check if FRootNode itself matches the class name
  if SameText(FRootNode.TagName, CName) then
  begin
    // FRootNode is the target node
    if Obj is TCollection then
      ReadCollection(TCollection(Obj), FRootNode)
    else
      ReadProperties(Obj, FRootNode);
    Result := True;
  end
  else
  begin
    // FRootNode is not the target, search in its children
    NodeList := FRootNode.GetElementsByTagName(CName);
    try
      if NodeList.Count > 0 then
      begin
        ObjNode := TCnXMLElement(NodeList[0]);

        // Uses node name as class name, no ClassType attribute
        if Obj is TCollection then
          ReadCollection(TCollection(Obj), ObjNode)
        else
          ReadProperties(Obj, ObjNode);

        Result := True;
      end;
    finally
      NodeList.Free;
    end;
  end;
end;

procedure TCnXMLReader.LoadFromFile(const FileName: string);
begin
  if FOwnsDocument and (FDocument <> nil) then
    FDocument.Free;

  FDocument := TCnXMLDocument.Create;
  FOwnsDocument := True;
  FDocument.LoadFromFile(FileName);

  FindRootNode;
end;

end.
