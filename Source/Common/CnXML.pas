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
  {* XML parsing and operation exception class}
  private
    FErrorCode: Integer;
    FLine: Integer;
    FColumn: Integer;
  public
    constructor Create(const Msg: string; AErrorCode, ALine, AColumn: Integer);
    {* Constructor
     |<PRE>
       Msg: string           - Error message
       AErrorCode: Integer   - Error code
       ALine: Integer        - Error line number
       AColumn: Integer      - Error column number
     |</PRE>}
    
    property ErrorCode: Integer read FErrorCode;
    {* Error code}
    property Line: Integer read FLine;
    {* Error line number}
    property Column: Integer read FColumn;
    {* Error column number}
  end;

  TCnXMLNodeType = (
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
  {* XML node type enumeration}

  TCnXMLTokenType = (
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
  {* XML token type enumeration}

//==============================================================================
// Error Code Constants
//==============================================================================

const
  // Lexical analysis errors
  CN_XML_ERR_INVALID_CHAR = 1;           // Invalid character
  CN_XML_ERR_UNEXPECTED_EOF = 2;         // Unexpected end of file
  CN_XML_ERR_INVALID_NAME = 3;           // Invalid name
  CN_XML_ERR_MISSING_QUOTE = 4;          // Missing quote
  CN_XML_ERR_INVALID_ENTITY = 5;         // Invalid entity reference
  
  // Syntax analysis errors
  CN_XML_ERR_TAG_MISMATCH = 10;          // Tag mismatch
  CN_XML_ERR_MISSING_ROOT = 11;          // Missing root element
  CN_XML_ERR_MULTIPLE_ROOTS = 12;        // Multiple root elements
  CN_XML_ERR_INVALID_STRUCTURE = 13;     // Invalid structure
  
  // DOM operation errors
  CN_XML_ERR_HIERARCHY = 20;             // Hierarchy error
  CN_XML_ERR_NOT_FOUND = 21;             // Node not found
  CN_XML_ERR_INVALID_OPERATION = 22;     // Invalid operation
  
  // Encoding errors
  CN_XML_ERR_ENCODING = 30;              // Encoding error
  CN_XML_ERR_INVALID_ENCODING = 31;      // Invalid encoding

  // Encoding constants
  CN_XML_ENCODING_UNKNOWN = 0;
  CN_XML_ENCODING_GBK = 1;
  CN_XML_ENCODING_UTF8 = 2;
  CN_XML_ENCODING_UTF16LE = 3;
  CN_XML_ENCODING_UTF16BE = 4;

//==============================================================================
// String Constants
//==============================================================================

resourcestring
  SCN_XML_INVALID_CHAR = 'XML Parsing Error: invalid character';
  SCN_XML_UNEXPECTED_EOF = 'XML Parsing Error: unexpected end of file';
  SCN_XML_INVALID_NAME = 'XML Parsing Error: invalid name';
  SCN_XML_MISSING_QUOTE = 'XML Parsing Error: missing quote';
  SCN_XML_INVALID_ENTITY = 'XML Parsing Error: invalid entity reference';
  SCN_XML_TAG_MISMATCH = 'XML Parsing Error: tag mismatch';
  SCN_XML_MISSING_ROOT = 'XML Parsing Error: missing root element';
  SCN_XML_MULTIPLE_ROOTS = 'XML Parsing Error: multiple root elements';
  SCN_XML_INVALID_STRUCTURE = 'XML Parsing Error: invalid structure';
  SCN_XML_HIERARCHY = 'XML Parsing Error: hierarchy error';
  SCN_XML_NOT_FOUND = 'XML Parsing Error: node not found';
  SCN_XML_INVALID_OPERATION = 'XML Parsing Error: invalid operation';
  SCN_XML_ENCODING = 'XML Encoding Error';
  SCN_XML_INVALID_ENCODING = 'XML Parsing Error: invalid encoding';

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
  {* XML token record type}
    TokenType: TCnXMLTokenType;
    {* Token type}
    Value: string;
    {* Token text value}
    Line: Integer;
    {* Line number}
    Column: Integer;
    {* Column number}
    Attributes: TStringList;
    {* Attribute list (for tags only)}
  end;

  TCnXMLLexer = class
  {* XML lexical analyzer}
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
    function ReadName: string;  // Read XML name
    function ReadAttributeValue: string;  // Read attribute value
    function ReadText: string;  // Read text content
    function ReadComment: string;  // Read comment
    function ReadCData: string;  // Read CDATA
    function ReadPI: string;  // Read processing instruction
  public
    constructor Create(const ASource: string);
    {* Constructor
     |<PRE>
       ASource: string - Source XML text
     |</PRE>}
    destructor Destroy; override;
    {* Destructor}
    
    function NextToken: TCnXMLToken;  // Get next token
    function CurrentPosition: Integer;  // Current position
    function CurrentLine: Integer;  // Current line number
    function CurrentColumn: Integer;  // Current column number
    function UnescapeText(const Text: string): string;  // Decode entity references (public for testing)
  end;

//==============================================================================
// DOM Base Classes
//==============================================================================

  TCnXMLNode = class
  {* XML node base class}
  private
    FNodeType: TCnXMLNodeType;
    FNodeName: string;
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
    procedure AddChild(Node: TCnXMLNode);
    procedure InternalRemoveChild(Node: TCnXMLNode);
    function IndexOfChild(Node: TCnXMLNode): Integer;
  public
    constructor Create(AOwnerDocument: TCnXMLDocument; ANodeType: TCnXMLNodeType);
    {* Constructor
     |<PRE>
       AOwnerDocument: TCnXMLDocument - Owner document
       ANodeType: TCnXMLNodeType      - Node type
     |</PRE>}
    destructor Destroy; override;
    {* Destructor}
    
    // Node operations
    function AppendChild(NewChild: TCnXMLNode): TCnXMLNode;
    {* Append a child node
     |<PRE>
       NewChild: TCnXMLNode - New child node
       Return: TCnXMLNode   - The appended child node
     |</PRE>}
    function InsertBefore(NewChild, RefChild: TCnXMLNode): TCnXMLNode;
    {* Insert a node before a reference node
     |<PRE>
       NewChild: TCnXMLNode  - New child node
       RefChild: TCnXMLNode  - Reference child node
       Return: TCnXMLNode    - The inserted child node
     |</PRE>}
    function RemoveChild(OldChild: TCnXMLNode): TCnXMLNode;
    {* Remove a child node
     |<PRE>
       OldChild: TCnXMLNode - Child node to remove
       Return: TCnXMLNode   - The removed child node
     |</PRE>}
    function ReplaceChild(NewChild, OldChild: TCnXMLNode): TCnXMLNode;
    {* Replace a child node
     |<PRE>
       NewChild: TCnXMLNode - New child node
       OldChild: TCnXMLNode - Old child node to replace
       Return: TCnXMLNode   - The replaced child node
     |</PRE>}
    function CloneNode(Deep: Boolean): TCnXMLNode;
    {* Clone a node
     |<PRE>
       Deep: Boolean      - Whether to clone child nodes recursively
       Return: TCnXMLNode - The cloned node
     |</PRE>}
    function HasChildNodes: Boolean;
    {* Check if the node has child nodes
     |<PRE>
       Return: Boolean - True if has child nodes, False otherwise
     |</PRE>}
    
    // Properties
    property NodeType: TCnXMLNodeType read FNodeType;
    {* Node type}
    property NodeName: string read FNodeName write FNodeName;
    {* Node name}
    property NodeValue: string read FNodeValue write FNodeValue;
    {* Node value}
    property ParentNode: TCnXMLNode read FParentNode;
    {* Parent node}
    property OwnerDocument: TCnXMLDocument read FOwnerDocument;
    {* Owner document}
    property FirstChild: TCnXMLNode read GetFirstChild;
    {* First child node}
    property LastChild: TCnXMLNode read GetLastChild;
    {* Last child node}
    property NextSibling: TCnXMLNode read GetNextSibling;
    {* Next sibling node}
    property PreviousSibling: TCnXMLNode read GetPreviousSibling;
    {* Previous sibling node}
    property ChildCount: Integer read GetChildCount;
    {* Child node count}
    property Children[Index: Integer]: TCnXMLNode read GetChild;
    {* Child node by index}
    property Text: string read GetText write SetText;
    {* Node text content}
  end;

  TCnXMLAttribute = class
  {* XML attribute class}
  private
    FName: string;
    FValue: string;
    FOwnerElement: TCnXMLElement;
  public
    constructor Create(AOwnerElement: TCnXMLElement; const AName, AValue: string);
    {* Constructor
     |<PRE>
       AOwnerElement: TCnXMLElement - Owner element
       AName: string                - Attribute name
       AValue: string               - Attribute value
     |</PRE>}
    
    property Name: string read FName write FName;
    {* Attribute name}
    property Value: string read FValue write FValue;
    {* Attribute value}
    property OwnerElement: TCnXMLElement read FOwnerElement;
    {* Owner element}
  end;

  TCnXMLElement = class(TCnXMLNode)
  {* XML element class}
  private
    FAttributes: TList;  // List of attributes
    
    function GetAttributeCount: Integer;
    function GetAttributeName(Index: Integer): string;
    function GetAttributeValue(Index: Integer): string;
  public
    constructor Create(AOwnerDocument: TCnXMLDocument; const ATagName: string);
    {* Constructor
     |<PRE>
       AOwnerDocument: TCnXMLDocument - Owner document
       ATagName: string               - Tag name
     |</PRE>}
    destructor Destroy; override;
    {* Destructor}
    
    // Attribute operations
    function GetAttribute(const Name: string): string;
    {* Get attribute value by name
     |<PRE>
       Name: string   - Attribute name
       Return: string - Attribute value, empty string if not found
     |</PRE>}
    procedure SetAttribute(const Name, Value: string);
    {* Set attribute value
     |<PRE>
       Name: string  - Attribute name
       Value: string - Attribute value
     |</PRE>}
    function HasAttribute(const Name: string): Boolean;
    {* Check if attribute exists
     |<PRE>
       Name: string    - Attribute name
       Return: Boolean - True if exists, False otherwise
     |</PRE>}
    procedure RemoveAttribute(const Name: string);
    {* Remove attribute
     |<PRE>
       Name: string - Attribute name
     |</PRE>}
    function GetAttributeNode(const Name: string): TCnXMLAttribute;
    {* Get attribute node by name
     |<PRE>
       Name: string              - Attribute name
       Return: TCnXMLAttribute   - Attribute node, nil if not found
     |</PRE>}
    
    // Element query
    function GetElementsByTagName(const TagName: string): TList;
    {* Get elements by tag name
     |<PRE>
       TagName: string - Tag name
       Return: TList   - List of elements (caller must free the list but not the elements)
     |</PRE>}
    
    // Properties
    property TagName: string read FNodeName write FNodeName;
    {* Tag name}
    property AttributeCount: Integer read GetAttributeCount;
    {* Attribute count}
    property AttributeNames[Index: Integer]: string read GetAttributeName;
    {* Attribute name by index}
    property AttributeValues[Index: Integer]: string read GetAttributeValue;
    {* Attribute value by index}
  end;

  TCnXMLDocument = class(TCnXMLNode)
  {* XML document class}
  private
    FDocumentElement: TCnXMLElement;
    FEncoding: string;
    FVersion: string;
    FStandalone: string;
    FPreserveWhitespace: Boolean;
  public
    constructor Create;
    {* Constructor}
    destructor Destroy; override;
    {* Destructor}
    
    // Node creation
    function CreateElement(const TagName: string): TCnXMLElement;
    {* Create element node
     |<PRE>
       TagName: string        - Tag name
       Return: TCnXMLElement  - Created element node
     |</PRE>}
    function CreateTextNode(const Data: string): TCnXMLNode;
    {* Create text node
     |<PRE>
       Data: string       - Text data
       Return: TCnXMLNode - Created text node
     |</PRE>}
    function CreateComment(const Data: string): TCnXMLNode;
    {* Create comment node
     |<PRE>
       Data: string       - Comment data
       Return: TCnXMLNode - Created comment node
     |</PRE>}
    function CreateCDATASection(const Data: string): TCnXMLNode;
    {* Create CDATA section node
     |<PRE>
       Data: string       - CDATA data
       Return: TCnXMLNode - Created CDATA node
     |</PRE>}
    function CreateProcessingInstruction(const Target, Data: string): TCnXMLNode;
    {* Create processing instruction node
     |<PRE>
       Target: string     - PI target
       Data: string       - PI data
       Return: TCnXMLNode - Created PI node
     |</PRE>}
    
    // Override AppendChild to auto-set DocumentElement
    function AppendChild(NewChild: TCnXMLNode): TCnXMLNode;
    {* Append a child node and auto-set DocumentElement if needed
     |<PRE>
       NewChild: TCnXMLNode - New child node
       Return: TCnXMLNode   - The appended child node
     |</PRE>}
    
    // Document loading
    procedure LoadFromFile(const FileName: string);
    {* Load XML from file
     |<PRE>
       FileName: string - File name
     |</PRE>}
    procedure LoadFromStream(Stream: TStream);
    {* Load XML from stream
     |<PRE>
       Stream: TStream - Stream object
     |</PRE>}
    procedure LoadFromString(const XMLString: string);
    {* Load XML from string
     |<PRE>
       XMLString: string - XML string
     |</PRE>}
    
    // Document saving
    procedure SaveToFile(const FileName: string; Indent: Boolean);
    {* Save XML to file
     |<PRE>
       FileName: string - File name
       Indent: Boolean  - Whether to use indentation
     |</PRE>}
    procedure SaveToStream(Stream: TStream; Indent: Boolean);
    {* Save XML to stream
     |<PRE>
       Stream: TStream - Stream object
       Indent: Boolean - Whether to use indentation
     |</PRE>}
    function SaveToString(Indent: Boolean): string;
    {* Save XML to string
     |<PRE>
       Indent: Boolean - Whether to use indentation
       Return: string  - XML string
     |</PRE>}
    
    // Properties
    property DocumentElement: TCnXMLElement read FDocumentElement write FDocumentElement;
    {* Document root element}
    property Encoding: string read FEncoding write FEncoding;
    {* Document encoding}
    property Version: string read FVersion write FVersion;
    {* XML version}
    property Standalone: string read FStandalone write FStandalone;
    {* Standalone declaration}
    property PreserveWhitespace: Boolean read FPreserveWhitespace write FPreserveWhitespace;
    {* Whether to preserve whitespace}
  end;

//==============================================================================
// Syntax Analyzer (Parser)
//==============================================================================

  TCnXMLParser = class
  {* XML syntax analyzer}
  private
    FLexer: TCnXMLLexer;
    FDocument: TCnXMLDocument;
    FCurrentToken: TCnXMLToken;
    
    procedure NextToken;  // Get next token
    procedure Expect(TokenType: TCnXMLTokenType);  // Expect specific token type
    procedure RaiseError(const Msg: string);  // Raise parsing error
    procedure ParseXMLDecl;  // Parse XML declaration
    procedure ParseElement(ParentNode: TCnXMLNode);  // Parse element
    procedure ParseContent(ParentNode: TCnXMLNode);  // Parse content
  public
    constructor Create(const ASource: string);
    {* Constructor
     |<PRE>
       ASource: string - Source XML text
     |</PRE>}
    destructor Destroy; override;
    {* Destructor}
    
    function Parse: TCnXMLDocument;  // Execute parsing, return document object
  end;

//==============================================================================
// Encoding Functions
//==============================================================================

function CnXMLDetectEncoding(const Buffer: TBytes): Integer;
{* Detect encoding from byte buffer
 |<PRE>
   Buffer: TBytes - Byte buffer
   Return: Integer - Encoding constant (CN_XML_ENCODING_*)
 |</PRE>}

function CnXMLConvertEncoding(const Source: AnsiString; 
  SourceEncoding, TargetEncoding: Integer): AnsiString;
{* Convert encoding (internal use AnsiString)
 |<PRE>
   Source: AnsiString         - Source string
   SourceEncoding: Integer    - Source encoding constant
   TargetEncoding: Integer    - Target encoding constant
   Return: AnsiString         - Converted string
 |</PRE>}

{$IFDEF UNICODE}
function CnXMLUtf8ToUnicode(const S: AnsiString): string;
{* Convert UTF-8 AnsiString to UnicodeString
 |<PRE>
   S: AnsiString  - UTF-8 encoded AnsiString
   Return: string - UnicodeString
 |</PRE>}

function CnXMLUnicodeToUtf8(const S: string): AnsiString;
{* Convert UnicodeString to UTF-8 AnsiString
 |<PRE>
   S: string          - UnicodeString
   Return: AnsiString - UTF-8 encoded AnsiString
 |</PRE>}
{$ENDIF}

//==============================================================================
// Object Serialization Classes
//==============================================================================

type
  TCnXMLWriter = class(TComponent)
  {* XML object writer for serialization}
  private
    FDocument: TCnXMLDocument;
    FRootNode: TCnXMLElement;
    FEncoding: string;
    FOwnsDocument: Boolean;
    procedure WriteProperties(const Obj: TPersistent; Node: TCnXMLElement);
    procedure WriteCollection(const Collection: TCollection; Node: TCnXMLElement);
    procedure SetClassType(const Obj: TPersistent; Node: TCnXMLElement);
  protected
    function GetXMLString: string;
    procedure SetXMLString(const Value: string);
    procedure InitDocument;
  public
    constructor Create(AOwner: TComponent); override;
    {* Constructor}
    destructor Destroy; override;
    {* Destructor}
    
    procedure WriteObjectToXML(const ObjectName: string; const Obj: TPersistent);
    {* Write object to XML
     |<PRE>
       ObjectName: string - Object node name
       Obj: TPersistent   - Object to serialize
     |</PRE>}
    procedure SaveToFile(const FileName: string);
    {* Save XML to file
     |<PRE>
       FileName: string - File name
     |</PRE>}
    
    property Document: TCnXMLDocument read FDocument;
    {* XML document}
    property RootNode: TCnXMLElement read FRootNode;
    {* Root node}
  published
    property Encoding: string read FEncoding write FEncoding;
    {* XML encoding}
    property XMLString: string read GetXMLString write SetXMLString;
    {* XML string}
  end;

  TCnXMLReader = class(TComponent)
  {* XML object reader for deserialization}
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
    {* Constructor}
    destructor Destroy; override;
    {* Destructor}
    
    function ReadObjectFromXML(const ObjectName: string; Obj: TPersistent): Boolean;
    {* Read object from XML
     |<PRE>
       ObjectName: string - Object node name
       Obj: TPersistent   - Object to deserialize into
       Return: Boolean    - True if successful
     |</PRE>}
    procedure LoadFromFile(const FileName: string);
    {* Load XML from file
     |<PRE>
       FileName: string - File name
     |</PRE>}
    
    property Document: TCnXMLDocument read FDocument;
    {* XML document}
    property RootNode: TCnXMLElement read FRootNode;
    {* Root node}
  published
    property Encoding: string read FEncoding write FEncoding;
    {* XML encoding}
    property XMLString: string read GetXMLString write SetXMLString;
    {* XML string}
  end;

//==============================================================================
// Utility Functions
//==============================================================================

function CnXMLCreateDocument: TCnXMLDocument;
{* Create a new XML document
 |<PRE>
   Return: TCnXMLDocument - New document object
 |</PRE>}

implementation

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
  // Check for UTF-16 BOM (FEFF) or UTF-8 BOM that wasn't stripped
  if (FLength >= 1) and (FSource[1] = #$FEFF) then
  begin
    FPosition := 1;  // Skip UTF-16 BOM
  end;
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
  
  // Trim leading and trailing whitespace
  Result := Trim(Result);
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
          CharCode := 0;
          
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
begin
  // Initialize token
  Result.TokenType := xttNone;
  Result.Value := '';
  Result.Line := FLine;
  Result.Column := FColumn;
  Result.Attributes := nil;
  
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
begin
  // Create new node with same type
  Result := TCnXMLNode.Create(FOwnerDocument, FNodeType);
  Result.NodeName := FNodeName;
  Result.NodeValue := FNodeValue;
  
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
  StringStream: TStringStream;
  XMLString: string;
begin
{$IFDEF UNICODE}
  StringStream := TStringStream.Create('', TEncoding.UTF8);
{$ELSE}
  StringStream := TStringStream.Create('');
{$ENDIF}
  try
    StringStream.CopyFrom(Stream, 0);
    XMLString := StringStream.DataString;
    LoadFromString(XMLString);
  finally
    StringStream.Free;
  end;
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
var
  IndentLevel: Integer;
  
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
            if Indent then
              Output := Output + #13#10;
            
            // Serialize children
            for I := 0 to Element.ChildCount - 1 do
              SerializeNode(Element.Children[I], Level + 1, Output);
            
            // End tag
            Output := Output + GetIndentString(Level) + '</' + Element.TagName + '>';
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
          if Trim(Node.NodeValue) <> '' then
          begin
            Output := Output + GetIndentString(Level) + EscapeText(Node.NodeValue);
            if Indent then
              Output := Output + #13#10;
          end;
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
  IndentLevel := 0;
  
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
  StringStream: TStringStream;
begin
  XMLString := SaveToString(Indent);
{$IFDEF UNICODE}
  StringStream := TStringStream.Create(XMLString, TEncoding.UTF8);
{$ELSE}
  StringStream := TStringStream.Create(XMLString);
{$ENDIF}
  try
    Stream.CopyFrom(StringStream, 0);
  finally
    StringStream.Free;
  end;
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

procedure TCnXMLParser.Expect(TokenType: TCnXMLTokenType);
begin
  if FCurrentToken.TokenType <> TokenType then
    RaiseError('Expected token type ' + IntToStr(Ord(TokenType)) + 
               ' but got ' + IntToStr(Ord(FCurrentToken.TokenType)));
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
  TextNode: TCnXMLNode;
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

{$IFDEF UNICODE}
function CnXMLUtf8ToUnicode(const S: AnsiString): string;
begin
  // Use Delphi's built-in UTF8ToString function
  // UTF8String is an alias for AnsiString with UTF-8 code page
  Result := UTF8ToString(UTF8String(S));
end;

function CnXMLUnicodeToUtf8(const S: string): AnsiString;
var
  UTF8Str: UTF8String;
begin
  // Use Delphi's built-in UTF8Encode function
  UTF8Str := UTF8Encode(S);
  Result := AnsiString(UTF8Str);
end;
{$ENDIF}

//==============================================================================
// Utility Functions Implementation
//==============================================================================

function CnXMLCreateDocument: TCnXMLDocument;
begin
  Result := TCnXMLDocument.Create;
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
  InitDocument;
end;

destructor TCnXMLWriter.Destroy;
begin
  if FOwnsDocument and (FDocument <> nil) then
    FDocument.Free;
  inherited;
end;

procedure TCnXMLWriter.InitDocument;
begin
  if FOwnsDocument and (FDocument <> nil) then
    FDocument.Free;
    
  FDocument := TCnXMLDocument.Create;
  FOwnsDocument := True;
  FDocument.Encoding := FEncoding;
  FDocument.Version := '1.0';
  
  // Create root node
  FRootNode := FDocument.CreateElement('XMLPersistent');
  FDocument.AppendChild(FRootNode);
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
  
  // Find root node
  if FDocument.DocumentElement <> nil then
    FRootNode := FDocument.DocumentElement
  else
  begin
    FRootNode := FDocument.CreateElement('XMLPersistent');
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
    ItemNode := FDocument.CreateElement('Item');
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
            Node.SetAttribute(string(PropInfo.Name), PropValue);
          end;
          
        tkSet:
          begin
            PropValue := GetSetProp(Obj, string(PropInfo.Name));
            Node.SetAttribute(string(PropInfo.Name), PropValue);
          end;
          
        tkInteger, tkChar, tkWChar, tkFloat, tkString, tkLString, 
        tkWString{$IFDEF UNICODE}, tkUString{$ENDIF}, tkVariant, tkInt64:
          begin
            PropValue := GetPropValue(Obj, string(PropInfo.Name));
            Node.SetAttribute(string(PropInfo.Name), PropValue);
          end;
      end;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

procedure TCnXMLWriter.WriteObjectToXML(const ObjectName: string; const Obj: TPersistent);
var
  ObjNode: TCnXMLElement;
  NodeList: TList;
begin
  if Obj = nil then
    Exit;
    
  // Find or create object node
  NodeList := FRootNode.GetElementsByTagName(ObjectName);
  try
    if NodeList.Count > 0 then
      ObjNode := TCnXMLElement(NodeList[0])
    else
      ObjNode := nil;
      
    if ObjNode = nil then
    begin
      ObjNode := FDocument.CreateElement(ObjectName);
      FRootNode.AppendChild(ObjNode);
    end
    else
    begin
      // Clear existing content
      while ObjNode.HasChildNodes do
        ObjNode.RemoveChild(ObjNode.FirstChild);
    end;
    
    ObjNode.SetAttribute('NODE_TYPE', 'OBJECT');
    SetClassType(Obj, ObjNode);
    
    if Obj is TCollection then
      WriteCollection(TCollection(Obj), ObjNode)
    else
      WriteProperties(Obj, ObjNode);
  finally
    NodeList.Free;
  end;
end;

procedure TCnXMLWriter.SaveToFile(const FileName: string);
begin
  if FDocument <> nil then
    FDocument.SaveToFile(FileName, False);
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
begin
  case PProp.PropType^.Kind of
    tkEnumeration:
      SetEnumProp(Obj, PropName, PropValue);
    tkSet:
      SetSetProp(Obj, PropName, PropValue);
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
    if (ItemNode.NodeType = xntElement) and (ItemNode.NodeName = 'Item') then
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
  I, J, PropCount, AttrCount: Integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropObj: TObject;
  PropValue: string;
  ChildNode: TCnXMLNode;
  AttrName: string;
begin
  PropCount := GetTypeData(Obj.ClassInfo)^.PropCount;
  if PropCount = 0 then
    Exit;
    
  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(Obj.ClassInfo, tkAny, PropList);
    
    // Read attributes (simple properties)
    AttrCount := Node.AttributeCount;
    for I := 0 to AttrCount - 1 do
    begin
      AttrName := Node.AttributeNames[I];
      
      // Skip special attributes
      if (AttrName = 'ClassType') or (AttrName = 'PersistentType') or 
         (AttrName = 'NODE_TYPE') then
        Continue;
        
      PropValue := Node.GetAttribute(AttrName);
      
      // Find matching property
      for J := 0 to PropCount - 1 do
      begin
        PropInfo := PropList^[J];
        if string(PropInfo.Name) = AttrName then
        begin
          SetPropertyValue(Obj, AttrName, PropValue, PropInfo);
          Break;
        end;
      end;
    end;
    
    // Read child nodes (complex properties)
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
            PropObj := GetObjectProp(Obj, string(PropInfo.Name));
            if PropObj <> nil then
            begin
              if PropObj is TCollection then
                ReadCollection(TCollection(PropObj), TCnXMLElement(ChildNode))
              else if PropObj is TPersistent then
                ReadProperties(TPersistent(PropObj), TCnXMLElement(ChildNode));
            end;
          end;
          Break;
        end;
      end;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

function TCnXMLReader.ReadObjectFromXML(const ObjectName: string; Obj: TPersistent): Boolean;
var
  ObjNode: TCnXMLElement;
  NodeList: TList;
begin
  Result := False;
  
  if (Obj = nil) or (FRootNode = nil) then
    Exit;
    
  // Find object node
  NodeList := FRootNode.GetElementsByTagName(ObjectName);
  try
    if NodeList.Count > 0 then
    begin
      ObjNode := TCnXMLElement(NodeList[0]);
      
      // Verify class type matches
      if ObjNode.GetAttribute('ClassType') = Obj.ClassName then
      begin
        if Obj is TCollection then
          ReadCollection(TCollection(Obj), ObjNode)
        else
          ReadProperties(Obj, ObjNode);
          
        Result := True;
      end;
    end;
  finally
    NodeList.Free;
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
