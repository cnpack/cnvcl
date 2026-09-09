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

unit CnMarkDown;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：MarkDown 格式解析单元
* 单元作者：CnPack 开发组
* 备    注：语法支持不完整，譬如没有表格，不支持嵌套列表等
*           Parser 能够实现 MarkDown 的词法分析，CnParseMarkDownString 则能将
*           MarkDown 文本解析成 DOM 树。该树包括两层结构，第一层是段落、第二层是文字。
*           根据 DOM 树未来可再输出成 HTML 或 RTF。
*           新增 UTF-16 Document/Block/Inline 流式模型，提供稳定块标识、
*           修订号、原文范围、活动尾块增量更新和完成通知，支持跨分片
*           CR/LF 及未闭合尾块固化。同时增加可配置资源限制和按文档、
*           单块生成或写入流的 Unicode RTF，支持主题字体、颜色、中文
*           和 emoji，并保持原有 Parser、DOM 及 RTF API 兼容。
*           配合 TCnMarkDownFeed 管理消息流，并由 TCnMarkDownView 负责虚拟化显示；
*           通过 Block 分页、按需解析和有限的 RichEdit 宿主池，支持 50 MB 历史、
*           10,000 条消息而不阻塞 UI 输入和滚动。
* 开发平台：PWin7 + Delphi 5
* 兼容测试：PWin7 + Delphi 2009 ~
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2026.09.09 V1.1
*               增加流式文档模型、增量解析与 Unicode RTF 输出
*           2025.03.06 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, Contnrs, TypInfo, CnStrings;

type
  TCnMarkDownTokenType = (cmtUnknown,
  {* MarkDown 的简单语法，不支持表格}
    cmtHeading1,       // 行首#空格
    cmtHeading2,       // 行首##空格
    cmtHeading3,       // 行首###空格
    cmtHeading4,       // 行首####空格
    cmtHeading5,       // 行首#####空格
    cmtHeading6,       // 行首######空格
    cmtHeading7,       // 行首#######空格
    cmtUnOrderedList,  // 行首*+-之一加空格
    cmtOrderedList,    // 行首数字.加空格
    cmtIndent,         // 行首四个空格或一个Tab
    cmtLine,           // 行首***或---或___行尾
    cmtQuota,          // 行首>空格
    cmtFenceCodeBlock, // 行首```
    cmtHardBreak,      // 俩空格行尾回车
    cmtCodeBlock,      // `
    cmtBold,           // ** 或 __
    cmtItalic,         // * 或 _
    cmtBoldItalic,     // *** 或 ___
    cmtStroke,         // ~~
    cmtLinkDisplay,    // [
    cmtLink,           // (
    cmtDirectLink,     // <
    cmtImageSign,      // !
    cmtContent,        // 内容
    cmtSpace,          // 空格
    cmtLineBreak,      // 回车换行
    cmtTerminate       // 结束符
  );
  TCnMarkDownTokenTypes = set of TCnMarkDownTokenType;

  TCnMarkDownBookmark = packed record
  {* 解析器中的书签，作为回溯用}
    Run: Integer;
    TokenPos: Integer;
    IsLineStart: Boolean;
    TokenID: TCnMarkDownTokenType;
  end;

  TCnMarkDownParser = class
  {* String 格式的 MarkDown 字符串语法解析器}
  private
    FRun: Integer;
    FTokenPos: Integer;
    FOrigin: PChar;
    FIsLineStart: Boolean;
    FTokenID: TCnMarkDownTokenType;

    procedure SharpHeaderProc;     // #     行首标题
    procedure NumberHeaderProc;    // 数字  行首有序列表
    procedure GreaterHeaderProc;   // >     行首引用
    procedure PlusHeaderProc;      // +     整行仨凑横线
    procedure MinusHeaderProc;     // -     整行仨凑横线
    procedure TabHeaderProc;       // Tab   单个缩进
    procedure UnderLineProc;       // _     整行仨凑横线，或粗斜体
    procedure SpaceProc;           // 空格  四个缩进或俩加回车
    procedure SquareProc;          // [和]之间是链接显示
    procedure LessProc;            // <和>之间是直接链接
    procedure BraceProc;           // (和)之间是链接跳转内容
    procedure ExclamationProc;     // !后[是图像
    procedure StarProc;            // * 仨凑横线或粗斜体
    procedure WaveProc;            // ~ 俩删除线
    procedure QuotaProc;           // `
    procedure LineBreakProc;       // 普通的回车换行
    procedure TerminateProc;       // #0 Next 时不会前进

    function GetToken: string;
    procedure SetOrigin(const Value: PChar);
    function GetTokenLength: Integer;
  protected
    procedure StepRun; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure Next;
    {* 跳至下一个 Token 并确定 TokenID}

    procedure SaveToBookmark(var Bookmark: TCnMarkDownBookmark);
    procedure LoadFromBookmark(var Bookmark: TCnMarkDownBookmark);

    property Origin: PChar read FOrigin write SetOrigin;
    {* 待解析的 string 格式的 MarkDown 字符串内容}
    property RunPos: Integer read FRun;
    {* 当前处理位置相对于 FOrigin 的线性偏移量，单位为字节数，0 开始}
    property TokenID: TCnMarkDownTokenType read FTokenID;
    {* 当前 Token 类型}
    property Token: string read GetToken;
    {* 当前 Token 的原始字符串，暂不解析转义内容}
    property TokenLength: Integer read GetTokenLength;
    {* 当前 Token 的字节长度}
  end;

  TCnMarkDownParagraphType = (cmpUnknown, cmpHeading1, cmpHeading2, cmpHeading3,
    cmpHeading4, cmpHeading5, cmpHeading6, cmpHeading7, cmpCommon, cmpPre, cmpLine,
    cmpFenceCodeBlock, cmpOrderedList, cmpUnorderedList, cmpQuota, cmpEmpty);
  {* 段落类型}

  TCnMarkDownTextFragmentType = (cmfUnknown, cmfCommon, cmfHardBreak, cmfBold, cmfItalic,
    cmfBoldItalic, cmfStroke, cmfCodeBlock, cmfFenceCodeBlockStart, cmfFenceCodeBlockEnd,
    cmfLink, cmfLinkDisplay, cmfImage, cmfDirectLink);
  {* 文本类型}
  TCnMarkDownTextFragmentTypes = set of TCnMarkDownTextFragmentType;

  TCnMarkDownTextBraceType = (cmtbNone, cmtbBold, cmtbItalic, cmtbBoldItalic, cmtbStroke,
    cmtbCodeBlock, cmtbFenceCodeBlock);
  {* 文本块需要配对的类型}

{$IFDEF UNICODE}
  TCnMarkDownText = string;
{$ELSE}
  TCnMarkDownText = WideString;
{$ENDIF}

  TCnMarkDownBlockType = (cmbUnknown, cmbParagraph, cmbHeading,
    cmbThematicBreak, cmbCodeBlock, cmbOrderedListItem,
    cmbUnorderedListItem);

  TCnMarkDownInlineType = (cmiText, cmiStrong, cmiEmphasis,
    cmiStrongEmphasis, cmiStrikethrough, cmiCode, cmiLink, cmiImage,
    cmiSoftBreak, cmiHardBreak);

  TCnMarkDownRTFStyle = class(TPersistent)
  private
    FFontName: TCnMarkDownText;
    FCodeFontName: TCnMarkDownText;
    FTextColor: Cardinal;
    FLinkColor: Cardinal;
    FCodeBackgroundColor: Cardinal;
    FSecondaryColor: Cardinal;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property FontName: TCnMarkDownText read FFontName write FFontName;
    property CodeFontName: TCnMarkDownText read FCodeFontName write FCodeFontName;
    property TextColor: Cardinal read FTextColor write FTextColor;
    property LinkColor: Cardinal read FLinkColor write FLinkColor;
    property CodeBackgroundColor: Cardinal read FCodeBackgroundColor
      write FCodeBackgroundColor;
    property SecondaryColor: Cardinal read FSecondaryColor write FSecondaryColor;
  end;

  TCnMarkDownDocumentChangeType = (cmdReset, cmdBlockAdded,
    cmdBlockChanged, cmdBlockCommitted, cmdFinished);

  TCnMarkDownDocumentChangeEvent = procedure(Sender: TObject;
    ChangeType: TCnMarkDownDocumentChangeType; BlockIndex: Integer) of object;

const
  CN_MARKDOWN_DEFAULT_MAX_SOURCE_LENGTH = 512 * 1024 * 1024;
  CN_MARKDOWN_DEFAULT_MAX_CHUNK_LENGTH = 16 * 1024 * 1024;
  CN_MARKDOWN_DEFAULT_MAX_LINE_LENGTH = 4 * 1024 * 1024;
  CN_MARKDOWN_DEFAULT_MAX_BLOCK_LENGTH = 32 * 1024 * 1024;
  CN_MARKDOWN_DEFAULT_MAX_BLOCK_COUNT = 100000;

type

  TCnMarkDownTextBuffer = class
  private
    FData: TCnMarkDownText;
    FLength: Integer;
    function GetText: TCnMarkDownText;
    procedure Grow(RequiredLength: Integer);
  public
    constructor Create;
    procedure Append(const Value: TCnMarkDownText);
    procedure AppendChar(Value: WideChar);
    procedure Clear;
    procedure ReleaseStorage;
    function ExtractPrefix(ACount: Integer): TCnMarkDownText;
    property Length: Integer read FLength;
    property Text: TCnMarkDownText read GetText;
  end;

  TCnMarkDownInline = class
  private
    FInlineType: TCnMarkDownInlineType;
    FText: TCnMarkDownText;
    FTarget: TCnMarkDownText;
    FSourceStart: Integer;
    FSourceLength: Integer;
  public
    constructor Create(AInlineType: TCnMarkDownInlineType;
      const AText, ATarget: TCnMarkDownText; ASourceStart,
      ASourceLength: Integer);
    property InlineType: TCnMarkDownInlineType read FInlineType;
    property Text: TCnMarkDownText read FText;
    property Target: TCnMarkDownText read FTarget;
    property SourceStart: Integer read FSourceStart;
    property SourceLength: Integer read FSourceLength;
  end;

  TCnMarkDownBlock = class
  private
    FBlockID: Int64;
    FRevision: Cardinal;
    FBlockType: TCnMarkDownBlockType;
    FText: TCnMarkDownText;
    FSourceStart: Int64;
    FSourceLength: Int64;
    FHeadingLevel: Integer;
    FQuoteLevel: Integer;
    FListLevel: Integer;
    FListStart: Integer;
    FCodeLanguage: TCnMarkDownText;
    FStable: Boolean;
    FInlines: TObjectList;
    function GetInline(Index: Integer): TCnMarkDownInline;
    function GetInlineCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddInline(AInline: TCnMarkDownInline);
    procedure ClearInlines;
    procedure RebuildInlines;
    property BlockID: Int64 read FBlockID;
    property Revision: Cardinal read FRevision;
    property BlockType: TCnMarkDownBlockType read FBlockType write FBlockType;
    property Text: TCnMarkDownText read FText write FText;
    property SourceStart: Int64 read FSourceStart write FSourceStart;
    property SourceLength: Int64 read FSourceLength write FSourceLength;
    property HeadingLevel: Integer read FHeadingLevel write FHeadingLevel;
    property QuoteLevel: Integer read FQuoteLevel write FQuoteLevel;
    property ListLevel: Integer read FListLevel write FListLevel;
    property ListStart: Integer read FListStart write FListStart;
    property CodeLanguage: TCnMarkDownText read FCodeLanguage write FCodeLanguage;
    property Stable: Boolean read FStable;
    property InlineCount: Integer read GetInlineCount;
    property Inlines[Index: Integer]: TCnMarkDownInline read GetInline;
  end;

  TCnMarkDownDocument = class
  private
    FBlocks: TObjectList;
    FSource: TCnMarkDownTextBuffer;
    FNextBlockID: Int64;
    FRevision: Cardinal;
    FActiveBlock: TCnMarkDownBlock;
    FMaxBlockCount: Integer;
    FKeepSource: Boolean;
    FOnChange: TCnMarkDownDocumentChangeEvent;
    function GetBlock(Index: Integer): TCnMarkDownBlock;
    function GetBlockCount: Integer;
    function GetSourceText: TCnMarkDownText;
    procedure Changed(ChangeType: TCnMarkDownDocumentChangeType;
      BlockIndex: Integer);
    procedure AppendSource(const Value: TCnMarkDownText);
    procedure UpdateActiveBlock(ABlock: TCnMarkDownBlock);
    procedure CommitActiveBlock;
    procedure AddStableBlock(ABlock: TCnMarkDownBlock);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function ActiveBlockIndex: Integer;
    property BlockCount: Integer read GetBlockCount;
    property Blocks[Index: Integer]: TCnMarkDownBlock read GetBlock;
    property SourceText: TCnMarkDownText read GetSourceText;
    property Revision: Cardinal read FRevision;
    property MaxBlockCount: Integer read FMaxBlockCount write FMaxBlockCount;
    property KeepSource: Boolean read FKeepSource write FKeepSource;
    property OnChange: TCnMarkDownDocumentChangeEvent read FOnChange write FOnChange;
  end;

  TCnMarkDownStreamParser = class
  private
    FDocument: TCnMarkDownDocument;
    FLineBuffer: TCnMarkDownTextBuffer;
    FCurrentBuffer: TCnMarkDownTextBuffer;
    FCurrentBlockType: TCnMarkDownBlockType;
    FCurrentSourceStart: Int64;
    FCurrentSourceEnd: Int64;
    FCurrentQuoteLevel: Integer;
    FCurrentLineCount: Integer;
    FCodeLanguage: TCnMarkDownText;
    FInFence: Boolean;
    FFenceChar: WideChar;
    FFenceLength: Integer;
    FPendingCR: Boolean;
    FScanOffset: Int64;
    FLineStartOffset: Int64;
    FFinished: Boolean;
    FBuildInlines: Boolean;
    FMaxSourceLength: Int64;
    FMaxChunkLength: Integer;
    FMaxLineLength: Integer;
    FMaxBlockLength: Integer;
    function GetMaxBlockCount: Integer;
    procedure SetMaxBlockCount(Value: Integer);
    procedure CheckBlockLength(Value: Int64);
    procedure ValidateChunk(const AChunk: TCnMarkDownText);
    procedure CommitCurrentBlock;
    procedure ProcessLine(const Line: TCnMarkDownText; ASourceStart,
      ASourceLength: Int64);
    procedure RefreshActiveBlock;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Append(const AChunk: TCnMarkDownText);
    procedure Finish;
    procedure Reset;
    property Document: TCnMarkDownDocument read FDocument;
    property Finished: Boolean read FFinished;
    property MaxSourceLength: Int64 read FMaxSourceLength write FMaxSourceLength;
    property MaxChunkLength: Integer read FMaxChunkLength write FMaxChunkLength;
    property MaxLineLength: Integer read FMaxLineLength write FMaxLineLength;
    property MaxBlockLength: Integer read FMaxBlockLength write FMaxBlockLength;
    property MaxBlockCount: Integer read GetMaxBlockCount write SetMaxBlockCount;
    property BuildInlines: Boolean read FBuildInlines write FBuildInlines;
  end;

  TCnVirtualHeightIndex = class
  private
    FHeights: array of Integer;
    FTree: array of Int64;
    FCount: Integer;
    FTotal: Int64;
    function GetHeight(Index: Integer): Integer;
    procedure SetHeight(Index: Integer; Value: Integer);
    procedure AddTree(Index, Delta: Integer);
  public
    constructor Create;
    procedure Clear;
    procedure SetCount(ACount, ADefaultHeight: Integer);
    procedure Append(AHeight: Integer);
    function PrefixHeight(ACount: Integer): Int64;
    function TopOf(Index: Integer): Int64;
    function BottomOf(Index: Integer): Int64;
    function IndexAtOffset(AOffset: Int64): Integer;
    property Count: Integer read FCount;
    property TotalHeight: Int64 read FTotal;
    property Heights[Index: Integer]: Integer read GetHeight write SetHeight;
  end;

  TCnMarkDownMessageRole = (cmrUser, cmrAssistant, cmrSystem, cmrTool);

  TCnMarkDownFeedChangeType = (cmfReset, cmfMessageAdded,
    cmfMessageChanged, cmfMessageFinished, cmfMessageQueued, cmfBatchChanged);

  TCnMarkDownFeedChangeEvent = procedure(Sender: TObject;
    ChangeType: TCnMarkDownFeedChangeType; MessageIndex: Integer) of object;

  TCnMarkDownFeedMessage = class
  private
    FID: Int64;
    FRole: TCnMarkDownMessageRole;
    FParser: TCnMarkDownStreamParser;
    FTitle: TCnMarkDownText;
    FPending: TCnMarkDownTextBuffer;
    function GetDocument: TCnMarkDownDocument;
    function GetFinished: Boolean;
    function GetKeepSource: Boolean;
    procedure SetKeepSource(Value: Boolean);
    function GetPendingLength: Integer;
  public
    constructor Create(AID: Int64; ARole: TCnMarkDownMessageRole);
    destructor Destroy; override;
    procedure Append(const AChunk: TCnMarkDownText);
    procedure Queue(const AChunk: TCnMarkDownText);
    function FlushQueued(AMaxChars: Integer): Integer;
    procedure Finish;
    procedure Reset;
    property ID: Int64 read FID;
    property Role: TCnMarkDownMessageRole read FRole write FRole;
    property Title: TCnMarkDownText read FTitle write FTitle;
    property Parser: TCnMarkDownStreamParser read FParser;
    property Document: TCnMarkDownDocument read GetDocument;
    property Finished: Boolean read GetFinished;
    property KeepSource: Boolean read GetKeepSource write SetKeepSource;
    property PendingLength: Integer read GetPendingLength;
  end;

  TCnMarkDownFeed = class
  private
    FMessages: TObjectList;
    FNextID: Int64;
    FUpdateCount: Integer;
    FMaxMessageCount: Integer;
    FKeepSourceText: Boolean;
    FOnChange: TCnMarkDownFeedChangeEvent;
    function GetMessage(Index: Integer): TCnMarkDownFeedMessage;
    function GetMessageCount: Integer;
    procedure Changed(ChangeType: TCnMarkDownFeedChangeType;
      MessageIndex: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AddMessage(ARole: TCnMarkDownMessageRole): TCnMarkDownFeedMessage;
    procedure AppendMessage(MessageIndex: Integer;
      const AChunk: TCnMarkDownText);
    procedure QueueMessage(MessageIndex: Integer;
      const AChunk: TCnMarkDownText);
    function FlushMessageQueue(MessageIndex, AMaxChars: Integer): Integer;
    procedure FinishMessage(MessageIndex: Integer);
    procedure BeginUpdate;
    procedure EndUpdate;
    property MessageCount: Integer read GetMessageCount;
    property Messages[Index: Integer]: TCnMarkDownFeedMessage read GetMessage;
    property MaxMessageCount: Integer read FMaxMessageCount write FMaxMessageCount;
    property KeepSourceText: Boolean read FKeepSourceText write FKeepSourceText;
    property NextID: Int64 read FNextID;
    property OnChange: TCnMarkDownFeedChangeEvent read FOnChange write FOnChange;
  end;

  TCnMarkDownBase = class
  {* 文法树节点的基类}
  private
    FItems: TObjectList;
    FParent: TCnMarkDownBase;
    function GetItem(Index: Integer): TCnMarkDownBase;
    procedure SetItem(Index: Integer; const Value: TCnMarkDownBase);
    function GetCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Add(AMarkDown: TCnMarkDownBase): TCnMarkDownBase;
    procedure Delete(Index: Integer);
    function Extract(Index: Integer): TCnMarkDownBase;

    property Parent: TCnMarkDownBase read FParent write FParent;
    property Items[Index: Integer]: TCnMarkDownBase read GetItem write SetItem; default;
    property Count: Integer read GetCount;
  end;

  TCnMarkDownParagraph = class(TCnMarkDownBase)
  {* 代表段落}
  private
    FParagraphType: TCnMarkDownParagraphType;
    FCloseType: TCnMarkDownTextBraceType;
    FOpenType: TCnMarkDownTextBraceType;
    FCodeType: string;
  public
    property OpenType: TCnMarkDownTextBraceType read FOpenType write FOpenType;
    property CloseType: TCnMarkDownTextBraceType read FCloseType write FCloseType;

    property ParagraphType: TCnMarkDownParagraphType read FParagraphType write FParagraphType;
    property CodeType: string read FCodeType write FCodeType;
    {* 类型为 cmpFenceCodeBlock 时的代码语言类型}
  end;

  TCnMarkDownTextFragment = class(TCnMarkDownBase)
  {* 代表段内文字块}
  private
    FContent: string;
    FFragmentType: TCnMarkDownTextFragmentType;
    FCloseType: TCnMarkDownTextBraceType;
    FOpenType: TCnMarkDownTextBraceType;
    function GetContent: string;
  public
    procedure AddContent(const Cont: string);

    property OpenType: TCnMarkDownTextBraceType read FOpenType write FOpenType;
    property CloseType: TCnMarkDownTextBraceType read FCloseType write FCloseType;

    property FragmentType: TCnMarkDownTextFragmentType read FFragmentType write FFragmentType;
    property Content: string read GetContent;
  end;

  TCnMarkDownConverter = class
  {* 转换器抽象基类}
  private
    FBasicFontSize: Integer;
  protected
    function ConvertParagraphStart(Paragraph: TCnMarkDownParagraph): string; virtual; abstract;
    function ConvertParagraphEnd(Paragraph: TCnMarkDownParagraph): string; virtual; abstract;
    function ConvertFragment(Fragment: TCnMarkDownTextFragment): string; virtual; abstract;
    function EscapeContent(const Text: string): string; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Convert(Root: TCnMarkDownBase): string; virtual; abstract;

    property BasicFontSize: Integer read FBasicFontSize write FBasicFontSize;
    {* 基础字体大小，单位是 Point，注意内部会转换成半 Point 为单位，也就是乘以 2}
  end;

  TCnRTFConverter = class(TCnMarkDownConverter)
  {* RTF 转换器实现类}
  private
    FRtf: TCnStringBuilder;
    FListCounters: array[1..9] of Integer; // 支持最多 9 级列表
    function GetListLevel(Paragraph: TCnMarkDownParagraph): Integer;
    function GetQuotaLevel(Paragraph: TCnMarkDownParagraph): Integer;
    {* 获得包括本节点在内的所有父节点层次中的引用嵌套数，0 开始}

    function PointToHalfPoint(Point: Integer): Integer;
    {* Point 转换为 RTF 中字体的尺寸单位半磅}
    function PointToTwips(Point: Integer): Integer;
    {* Point 转换为 RTF 中对齐的尺寸单位缇}
  protected
    function ConvertParagraphStart(Paragraph: TCnMarkDownParagraph): string; override;
    function ConvertParagraphEnd(Paragraph: TCnMarkDownParagraph): string; override;
    function ConvertFragment(Fragment: TCnMarkDownTextFragment): string; override;
    function EscapeContent(const Text: string): string; override;
    procedure ProcessNode(Node: TCnMarkDownBase);
  public
    constructor Create; override;
    destructor Destroy; override;
    function Convert(Root: TCnMarkDownBase): string; override;
  end;

function CnParseMarkDownString(const MarkDown: string): TCnMarkDownBase;
{* 将 MarkDown 字符串解析为树状对象，返回对象需在外部不用时释放}

procedure CnMarkDownDebugOutput(MarkDown: TCnMarkDownBase; List: TStrings);
{* 将 MarkDown 对象树打印到字符串列表中}

function CnMarkDownConvertToRTF(Root: TCnMarkDownBase; ABasicFontSize: Integer = 12): string;
{* 将 MarkDown 的 DOM 树输出成 RTF 字符串}

function CnMarkDownBlockToUnicodeRTF(Block: TCnMarkDownBlock;
  ABasicFontSize: Integer = 12; IncludeHeader: Boolean = True;
  AStyle: TCnMarkDownRTFStyle = nil): AnsiString;
function CnMarkDownDocumentToUnicodeRTF(Document: TCnMarkDownDocument;
  ABasicFontSize: Integer = 12;
  AStyle: TCnMarkDownRTFStyle = nil): AnsiString;
procedure CnMarkDownBlockToUnicodeRTFStream(AStream: TStream;
  Block: TCnMarkDownBlock; ABasicFontSize: Integer = 12;
  IncludeHeader: Boolean = True; AStyle: TCnMarkDownRTFStyle = nil);
procedure CnMarkDownDocumentToUnicodeRTFStream(AStream: TStream;
  Document: TCnMarkDownDocument; ABasicFontSize: Integer = 12;
  AStyle: TCnMarkDownRTFStyle = nil);

//  RTF 段落格式：{\pard [控制参数] [文本内容] \par}

implementation

resourcestring
  SCnMarkDownTextTooLong = 'The Markdown Text is Too Long to Process.';
  SCnMarkDownSourceTooLong = 'The Markdown Document Exceeds the Configured Length limit.';
  SCnMarkDownChunkTooLong = 'The Markdown Input Chunk Exceeds the Configured Length Limit.';
  SCnMarkDownLineTooLong = 'The Markdown Line Exceeds the Configured Length Limit.';
  SCnMarkDownBlockTooLong = 'The Markdown Block Exceeds the Configured Length Limit.';
  SCnMarkDownTooManyBlocks = 'The Markdown Document Exceeds the Configured Block Count Limit.';

const
  // 这些标记是需要配对的
  CN_MARKDOWN_FRAGMENTTYPE_NEED_MATCH: TCnMarkDownTextFragmentTypes =
    [cmfBold, cmfItalic, cmfBoldItalic, cmfStroke, cmfCodeBlock];

  // 遇见这些标记，即使前面没有连续两个回车换行，也要新起一段
  CN_MARKDOWN_TOKENTYPE_PARAHEAD: TCnMarkDownTokenTypes =
    [cmtHeading1,      // 行首#空格
    cmtHeading2,       // 行首##空格
    cmtHeading3,       // 行首###空格
    cmtHeading4,       // 行首####空格
    cmtHeading5,       // 行首#####空格
    cmtHeading6,       // 行首######空格
    cmtHeading7,       // 行首#######空格
    cmtUnOrderedList,  // 行首*+-之一加空格
    cmtOrderedList,    // 行首数字.加空格
    cmtIndent,         // 行首四个空格或一个Tab
    cmtLine,           // 行首***或---或___行尾
    cmtQuota,          // 行首>空格
    cmtFenceCodeBlock  // 行首```
  ];

  CN_RTF_HEADER =
    '{\rtf1\ansi\ansicpg936\deff0' +            // 文档头+简体中文代码页
    '{\fonttbl' +                               // 字体表开始
    '{\f0\fnil\fcharset134 SimSun;}' +          // 主字体：宋体（GB2312字符集）
    '{\f1\fnil\fcharset134 Microsoft YaHei;}' + // 备用字体1：微软雅黑
    '{\f2\fnil\fcharset134 KaiTi;}' +           // 备用字体2：楷体
    '{\f3\fnil\fcharset134 Courier New;}' +     // 等宽字体
    '}' +                                       // 字体表结束
    '{\colortbl;' +
    '\red0\green0\blue0;' +
    '\red255\green0\blue0;' +
    '\red216\green216\blue216;' +
    '\red204\green232\blue255;}' +              // 颜色表（黑、红、灰、浅蓝）
    '\viewkind4\uc1' +                          // 视图模式+Unicode声明
    '\pard' +
    '\lang2052\langfe2052\f0\fs%d\qj';          // 中文排版设置+默认字体

  CN_RTF_FOOTER = '}';

  CN_QUOTA_INDENT = 720;    // 引用的缩进
  CN_LIST_UNINDENT = -360;  // 列表的点或序号的反缩进
  CN_LIST_INDENT = 720;     // 列表文字的缩进

function CnMarkDownTextStartsAt(const Text, Value: TCnMarkDownText;
  Index: Integer): Boolean;
var
  I, L: Integer;
begin
  Result := False;
  L := Length(Value);
  if (Index < 1) or (L = 0) or (Index > Length(Text) - L + 1) then
    Exit;
  for I := 1 to L do
    if Text[Index + I - 1] <> Value[I] then
      Exit;
  Result := True;
end;

function CnMarkDownTextFind(const Text, Value: TCnMarkDownText;
  StartIndex: Integer): Integer;
var
  I, Last: Integer;
begin
  Result := 0;
  if StartIndex < 1 then
    StartIndex := 1;
  Last := Length(Text) - Length(Value) + 1;
  for I := StartIndex to Last do
    if CnMarkDownTextStartsAt(Text, Value, I) then
    begin
      Result := I;
      Exit;
    end;
end;

function CnMarkDownTextIsHorizontalSpace(C: WideChar): Boolean;
begin
  Result := (C = WideChar(' ')) or (C = WideChar(#9));
end;

function CnMarkDownTextIsTrimChar(C: WideChar): Boolean;
begin
  Result := CnMarkDownTextIsHorizontalSpace(C) or
    (C = WideChar(#13)) or (C = WideChar(#10));
end;

function CnMarkDownTextIsFenceChar(C: WideChar): Boolean;
begin
  Result := (C = WideChar('`')) or (C = WideChar('~'));
end;

function CnMarkDownTextIsThematicChar(C: WideChar): Boolean;
begin
  Result := (C = WideChar('*')) or (C = WideChar('-')) or
    (C = WideChar('_'));
end;

function CnMarkDownTextIsListChar(C: WideChar): Boolean;
begin
  Result := (C = WideChar('*')) or (C = WideChar('-')) or
    (C = WideChar('+'));
end;

function CnMarkDownTextIsDigit(C: WideChar): Boolean;
begin
  Result := (C >= WideChar('0')) and (C <= WideChar('9'));
end;

function CnMarkDownTextTrim(const Text: TCnMarkDownText): TCnMarkDownText;
var
  L, R: Integer;
begin
  L := 1;
  R := Length(Text);
  while (L <= R) and CnMarkDownTextIsTrimChar(Text[L]) do
    Inc(L);
  while (R >= L) and CnMarkDownTextIsTrimChar(Text[R]) do
    Dec(R);
  Result := Copy(Text, L, R - L + 1);
end;

function CnMarkDownTextIsBlank(const Text: TCnMarkDownText): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to Length(Text) do
    if not CnMarkDownTextIsHorizontalSpace(Text[I]) then
    begin
      Result := False;
      Exit;
    end;
end;

function CnMarkDownTryFence(const Text: TCnMarkDownText;
  var FenceChar: WideChar; var FenceLength: Integer;
  var Language: TCnMarkDownText): Boolean;
var
  I, N, SpaceCount: Integer;
begin
  Result := False;
  I := 1;
  SpaceCount := 0;
  while (I <= Length(Text)) and (Text[I] = WideChar(' ')) and
    (SpaceCount < 3) do
  begin
    Inc(I);
    Inc(SpaceCount);
  end;
  if (I > Length(Text)) or not CnMarkDownTextIsFenceChar(Text[I]) then
    Exit;
  FenceChar := Text[I];
  N := 0;
  while (I <= Length(Text)) and (Text[I] = FenceChar) do
  begin
    Inc(I);
    Inc(N);
  end;
  if N < 3 then
    Exit;
  FenceLength := N;
  Language := CnMarkDownTextTrim(Copy(Text, I, MaxInt));
  Result := True;
end;

function CnMarkDownIsFenceClose(const Text: TCnMarkDownText;
  FenceChar: WideChar; FenceLength: Integer): Boolean;
var
  I, N, SpaceCount: Integer;
begin
  Result := False;
  I := 1;
  SpaceCount := 0;
  while (I <= Length(Text)) and (Text[I] = WideChar(' ')) and
    (SpaceCount < 3) do
  begin
    Inc(I);
    Inc(SpaceCount);
  end;
  N := 0;
  while (I <= Length(Text)) and (Text[I] = FenceChar) do
  begin
    Inc(I);
    Inc(N);
  end;
  if N < FenceLength then
    Exit;
  while (I <= Length(Text)) and CnMarkDownTextIsHorizontalSpace(Text[I]) do
    Inc(I);
  Result := I > Length(Text);
end;

function CnMarkDownIsThematicBreak(const Text: TCnMarkDownText;
  StartIndex: Integer): Boolean;
var
  I, N: Integer;
  C: WideChar;
begin
  Result := False;
  if StartIndex > Length(Text) then
    Exit;
  C := Text[StartIndex];
  if not CnMarkDownTextIsThematicChar(C) then
    Exit;
  N := 0;
  for I := StartIndex to Length(Text) do
  begin
    if Text[I] = C then
      Inc(N)
    else if not CnMarkDownTextIsHorizontalSpace(Text[I]) then
      Exit;
  end;
  Result := N >= 3;
end;

function CnMarkDownCreateLineBlock(const Line: TCnMarkDownText;
  ASourceStart, ASourceLength: Int64): TCnMarkDownBlock;
var
  I, N, SpaceCount, QuoteLevel, ListLevel, ListStart: Integer;
  FenceLength: Integer;
  FenceChar: WideChar;
  Language: TCnMarkDownText;
begin
  Result := TCnMarkDownBlock.Create;
  Result.SourceStart := ASourceStart;
  Result.SourceLength := ASourceLength;
  Result.ListStart := 1;
  I := 1;
  SpaceCount := 0;
  while (I <= Length(Line)) and (Line[I] = WideChar(' ')) do
  begin
    Inc(I);
    Inc(SpaceCount);
  end;
  ListLevel := SpaceCount div 2 + 1;
  QuoteLevel := 0;
  while (I <= Length(Line)) and (Line[I] = '>') do
  begin
    Inc(QuoteLevel);
    Inc(I);
    if (I <= Length(Line)) and CnMarkDownTextIsHorizontalSpace(Line[I]) then
      Inc(I);
  end;
  Result.QuoteLevel := QuoteLevel;
  Result.ListLevel := ListLevel;

  FenceChar := #0;
  FenceLength := 0;
  Language := '';
  if (QuoteLevel = 0) and
    CnMarkDownTryFence(Line, FenceChar, FenceLength, Language) then
  begin
    Result.BlockType := cmbCodeBlock;
    Result.CodeLanguage := Language;
    Result.RebuildInlines;
    Exit;
  end;

  if I > Length(Line) then
  begin
    Result.BlockType := cmbUnknown;
    Exit;
  end;

  if Line[I] = '#' then
  begin
    N := 0;
    while (I + N <= Length(Line)) and (Line[I + N] = '#') and (N < 7) do
      Inc(N);
    if (N > 0) and (I + N <= Length(Line)) and
      CnMarkDownTextIsHorizontalSpace(Line[I + N]) then
    begin
      Result.BlockType := cmbHeading;
      Result.HeadingLevel := N;
      I := I + N;
      while (I <= Length(Line)) and CnMarkDownTextIsHorizontalSpace(Line[I]) do
        Inc(I);
      Result.Text := Copy(Line, I, MaxInt);
      Result.RebuildInlines;
      Exit;
    end;
  end;

  if CnMarkDownIsThematicBreak(Line, I) then
  begin
    Result.BlockType := cmbThematicBreak;
    Exit;
  end;

  if CnMarkDownTextIsListChar(Line[I]) and (I < Length(Line)) and
    CnMarkDownTextIsHorizontalSpace(Line[I + 1]) then
  begin
    Result.BlockType := cmbUnorderedListItem;
    Inc(I, 2);
    while (I <= Length(Line)) and CnMarkDownTextIsHorizontalSpace(Line[I]) do
      Inc(I);
    Result.Text := Copy(Line, I, MaxInt);
    Result.RebuildInlines;
    Exit;
  end;

  if CnMarkDownTextIsDigit(Line[I]) then
  begin
    ListStart := 0;
    N := I;
    while (N <= Length(Line)) and CnMarkDownTextIsDigit(Line[N]) do
    begin
      if ListStart <= 100000000 then
        ListStart := ListStart * 10 + Ord(Line[N]) - Ord('0');
      Inc(N);
    end;
    if (N + 1 <= Length(Line)) and (Line[N] = '.') and
      CnMarkDownTextIsHorizontalSpace(Line[N + 1]) then
    begin
      Result.BlockType := cmbOrderedListItem;
      Result.ListStart := ListStart;
      I := N + 2;
      while (I <= Length(Line)) and CnMarkDownTextIsHorizontalSpace(Line[I]) do
        Inc(I);
      Result.Text := Copy(Line, I, MaxInt);
      Result.RebuildInlines;
      Exit;
    end;
  end;

  Result.BlockType := cmbParagraph;
  Result.Text := Copy(Line, I, MaxInt);
  Result.RebuildInlines;
end;

constructor TCnMarkDownRTFStyle.Create;
begin
  inherited Create;
  FFontName := 'Segoe UI';
  FCodeFontName := 'Consolas';
  FTextColor := $000000;
  FLinkColor := $0066CC;
  FCodeBackgroundColor := $F0F0F0;
  FSecondaryColor := $606060;
end;

procedure TCnMarkDownRTFStyle.Assign(Source: TPersistent);
begin
  if Source is TCnMarkDownRTFStyle then
  begin
    FFontName := TCnMarkDownRTFStyle(Source).FontName;
    FCodeFontName := TCnMarkDownRTFStyle(Source).CodeFontName;
    FTextColor := TCnMarkDownRTFStyle(Source).TextColor;
    FLinkColor := TCnMarkDownRTFStyle(Source).LinkColor;
    FCodeBackgroundColor := TCnMarkDownRTFStyle(Source).CodeBackgroundColor;
    FSecondaryColor := TCnMarkDownRTFStyle(Source).SecondaryColor;
  end
  else
    inherited Assign(Source);
end;

constructor TCnMarkDownTextBuffer.Create;
begin
  inherited Create;
  FLength := 0;
end;

procedure TCnMarkDownTextBuffer.Grow(RequiredLength: Integer);
var
  NewCapacity: Integer;
begin
  NewCapacity := System.Length(FData);
  if NewCapacity < 64 then
    NewCapacity := 64;
  while NewCapacity < RequiredLength do
  begin
    if NewCapacity > MaxInt div 2 then
    begin
      NewCapacity := RequiredLength;
      Break;
    end;
    NewCapacity := NewCapacity * 2;
  end;
  SetLength(FData, NewCapacity);
end;

procedure TCnMarkDownTextBuffer.Append(const Value: TCnMarkDownText);
var
  L: Integer;
begin
  L := System.Length(Value);
  if L = 0 then
    Exit;
  if L > MaxInt - FLength then
    raise ERangeError.Create(SCnMarkDownTextTooLong);
  if FLength + L > System.Length(FData) then
    Grow(FLength + L);
  Move(Value[1], FData[FLength + 1], L * SizeOf(WideChar));
  Inc(FLength, L);
end;

procedure TCnMarkDownTextBuffer.AppendChar(Value: WideChar);
begin
  if FLength = MaxInt then
    raise ERangeError.Create(SCnMarkDownTextTooLong);
  if FLength + 1 > System.Length(FData) then
    Grow(FLength + 1);
  FData[FLength + 1] := Value;
  Inc(FLength);
end;

procedure TCnMarkDownTextBuffer.Clear;
begin
  FLength := 0;
end;

procedure TCnMarkDownTextBuffer.ReleaseStorage;
begin
  FLength := 0;
  SetLength(FData, 0);
end;

function TCnMarkDownTextBuffer.ExtractPrefix(ACount: Integer): TCnMarkDownText;
var
  Remaining: Integer;
begin
  if ACount < 0 then
    ACount := 0;
  if ACount > FLength then
    ACount := FLength;
  Result := Copy(FData, 1, ACount);
  Remaining := FLength - ACount;
  if Remaining > 0 then
    Move(FData[ACount + 1], FData[1], Remaining * SizeOf(WideChar));
  FLength := Remaining;
end;

function TCnMarkDownTextBuffer.GetText: TCnMarkDownText;
begin
  Result := Copy(FData, 1, FLength);
end;

constructor TCnMarkDownInline.Create(AInlineType: TCnMarkDownInlineType;
  const AText, ATarget: TCnMarkDownText; ASourceStart,
  ASourceLength: Integer);
begin
  inherited Create;
  FInlineType := AInlineType;
  FText := AText;
  FTarget := ATarget;
  FSourceStart := ASourceStart;
  FSourceLength := ASourceLength;
end;

constructor TCnMarkDownBlock.Create;
begin
  inherited Create;
  FInlines := TObjectList.Create(True);
  FListLevel := 1;
  FListStart := 1;
end;

destructor TCnMarkDownBlock.Destroy;
begin
  FInlines.Free;
  inherited Destroy;
end;

procedure TCnMarkDownBlock.AddInline(AInline: TCnMarkDownInline);
begin
  FInlines.Add(AInline);
end;

procedure TCnMarkDownBlock.ClearInlines;
begin
  FInlines.Clear;
end;

function TCnMarkDownBlock.GetInline(Index: Integer): TCnMarkDownInline;
begin
  Result := TCnMarkDownInline(FInlines[Index]);
end;

function TCnMarkDownBlock.GetInlineCount: Integer;
begin
  Result := FInlines.Count;
end;

procedure TCnMarkDownBlock.RebuildInlines;
var
  I, PlainStart, ClosePos, MidPos, BreakMarkerLength: Integer;
  CanParseLink, CanParseImage: Boolean;
  Delimiter: TCnMarkDownText;
  InlineType: TCnMarkDownInlineType;

  procedure AddPlain(EndIndex: Integer);
  begin
    if EndIndex >= PlainStart then
      AddInline(TCnMarkDownInline.Create(cmiText,
        Copy(FText, PlainStart, EndIndex - PlainStart + 1), '',
        PlainStart - 1, EndIndex - PlainStart + 1));
  end;

begin
  ClearInlines;
  if FText = '' then
    Exit;
  if FBlockType = cmbCodeBlock then
  begin
    AddInline(TCnMarkDownInline.Create(cmiText, FText, '', 0,
      Length(FText)));
    Exit;
  end;

  I := 1;
  PlainStart := 1;
  CanParseLink := True;
  CanParseImage := True;
  while I <= Length(FText) do
  begin
    if FText[I] = #10 then
    begin
      BreakMarkerLength := 0;
      InlineType := cmiSoftBreak;
      while (I - BreakMarkerLength > 1) and
        (FText[I - BreakMarkerLength - 1] = ' ') do
        Inc(BreakMarkerLength);
      if BreakMarkerLength < 2 then
      begin
        BreakMarkerLength := 0;
        if (I > 1) and (FText[I - 1] = '\') then
          BreakMarkerLength := 1;
      end;
      if BreakMarkerLength > 0 then
        InlineType := cmiHardBreak;
      AddPlain(I - BreakMarkerLength - 1);
      AddInline(TCnMarkDownInline.Create(InlineType, '', '',
        I - BreakMarkerLength - 1, BreakMarkerLength + 1));
      Inc(I);
      PlainStart := I;
      Continue;
    end;

    if CanParseImage and CnMarkDownTextStartsAt(FText, '![', I) then
    begin
      MidPos := CnMarkDownTextFind(FText, '](', I + 2);
      if MidPos > 0 then
      begin
        ClosePos := CnMarkDownTextFind(FText, ')', MidPos + 2);
        if ClosePos > 0 then
        begin
          AddPlain(I - 1);
          AddInline(TCnMarkDownInline.Create(cmiImage,
            Copy(FText, I + 2, MidPos - I - 2),
            Copy(FText, MidPos + 2, ClosePos - MidPos - 2), I - 1,
            ClosePos - I + 1));
          I := ClosePos + 1;
          PlainStart := I;
          Continue;
        end;
        CanParseImage := False;
      end;
      if MidPos = 0 then
        CanParseImage := False;
    end;

    if CanParseLink and (FText[I] = '[') then
    begin
      MidPos := CnMarkDownTextFind(FText, '](', I + 1);
      if MidPos > 0 then
      begin
        ClosePos := CnMarkDownTextFind(FText, ')', MidPos + 2);
        if ClosePos > 0 then
        begin
          AddPlain(I - 1);
          AddInline(TCnMarkDownInline.Create(cmiLink,
            Copy(FText, I + 1, MidPos - I - 1),
            Copy(FText, MidPos + 2, ClosePos - MidPos - 2), I - 1,
            ClosePos - I + 1));
          I := ClosePos + 1;
          PlainStart := I;
          Continue;
        end;
        CanParseLink := False;
      end;
      if MidPos = 0 then
        CanParseLink := False;
    end;

    Delimiter := '';
    InlineType := cmiText;
    if CnMarkDownTextStartsAt(FText, '***', I) then
    begin
      Delimiter := '***';
      InlineType := cmiStrongEmphasis;
    end
    else if CnMarkDownTextStartsAt(FText, '___', I) then
    begin
      Delimiter := '___';
      InlineType := cmiStrongEmphasis;
    end
    else if CnMarkDownTextStartsAt(FText, '**', I) then
    begin
      Delimiter := '**';
      InlineType := cmiStrong;
    end
    else if CnMarkDownTextStartsAt(FText, '__', I) then
    begin
      Delimiter := '__';
      InlineType := cmiStrong;
    end
    else if CnMarkDownTextStartsAt(FText, '~~', I) then
    begin
      Delimiter := '~~';
      InlineType := cmiStrikethrough;
    end
    else if FText[I] = '`' then
    begin
      Delimiter := '`';
      InlineType := cmiCode;
    end
    else if FText[I] = '*' then
    begin
      Delimiter := '*';
      InlineType := cmiEmphasis;
    end
    else if FText[I] = '_' then
    begin
      Delimiter := '_';
      InlineType := cmiEmphasis;
    end;

    if Delimiter <> '' then
    begin
      ClosePos := CnMarkDownTextFind(FText, Delimiter,
        I + Length(Delimiter));
      if ClosePos > 0 then
      begin
        AddPlain(I - 1);
        AddInline(TCnMarkDownInline.Create(InlineType,
          Copy(FText, I + Length(Delimiter),
            ClosePos - I - Length(Delimiter)), '', I - 1,
          ClosePos - I + Length(Delimiter)));
        I := ClosePos + Length(Delimiter);
        PlainStart := I;
        Continue;
      end;
    end;
    Inc(I);
  end;
  AddPlain(Length(FText));
end;

constructor TCnMarkDownDocument.Create;
begin
  inherited Create;
  FBlocks := TObjectList.Create(True);
  FSource := TCnMarkDownTextBuffer.Create;
  FNextBlockID := 1;
  FMaxBlockCount := CN_MARKDOWN_DEFAULT_MAX_BLOCK_COUNT;
  FKeepSource := True;
end;

destructor TCnMarkDownDocument.Destroy;
begin
  FSource.Free;
  FBlocks.Free;
  inherited Destroy;
end;

procedure TCnMarkDownDocument.Changed(
  ChangeType: TCnMarkDownDocumentChangeType; BlockIndex: Integer);
begin
  Inc(FRevision);
  if Assigned(FOnChange) then
    FOnChange(Self, ChangeType, BlockIndex);
end;

procedure TCnMarkDownDocument.Clear;
begin
  FActiveBlock := nil;
  FBlocks.Clear;
  FSource.Clear;
  FNextBlockID := 1;
  Changed(cmdReset, -1);
end;

function TCnMarkDownDocument.GetBlock(Index: Integer): TCnMarkDownBlock;
begin
  Result := TCnMarkDownBlock(FBlocks[Index]);
end;

function TCnMarkDownDocument.GetBlockCount: Integer;
begin
  Result := FBlocks.Count;
end;

function TCnMarkDownDocument.GetSourceText: TCnMarkDownText;
begin
  Result := FSource.Text;
end;

function TCnMarkDownDocument.ActiveBlockIndex: Integer;
begin
  if FActiveBlock <> nil then
    Result := FBlocks.Count - 1
  else
    Result := -1;
end;

procedure TCnMarkDownDocument.AppendSource(const Value: TCnMarkDownText);
begin
  if FKeepSource then
    FSource.Append(Value);
end;

procedure TCnMarkDownDocument.UpdateActiveBlock(ABlock: TCnMarkDownBlock);
var
  Index: Integer;
begin
  if ABlock = nil then
    Exit;
  ABlock.FStable := False;
  if FActiveBlock = nil then
  begin
    if (FMaxBlockCount > 0) and (FBlocks.Count >= FMaxBlockCount) then
    begin
      ABlock.Free;
      raise ERangeError.Create(SCnMarkDownTooManyBlocks);
    end;
    ABlock.FBlockID := FNextBlockID;
    Inc(FNextBlockID);
    ABlock.FRevision := 1;
    FBlocks.Add(ABlock);
    FActiveBlock := ABlock;
    Changed(cmdBlockAdded, FBlocks.Count - 1);
  end
  else
  begin
    Index := FBlocks.Count - 1;
    ABlock.FBlockID := FActiveBlock.FBlockID;
    ABlock.FRevision := FActiveBlock.FRevision + 1;
    FBlocks[Index] := ABlock;
    FActiveBlock := ABlock;
    Changed(cmdBlockChanged, Index);
  end;
end;

procedure TCnMarkDownDocument.CommitActiveBlock;
var
  Index: Integer;
begin
  if FActiveBlock = nil then
    Exit;
  Index := FBlocks.Count - 1;
  FActiveBlock.FStable := True;
  Inc(FActiveBlock.FRevision);
  FActiveBlock := nil;
  Changed(cmdBlockCommitted, Index);
end;

procedure TCnMarkDownDocument.AddStableBlock(ABlock: TCnMarkDownBlock);
var
  Index: Integer;
begin
  if ABlock = nil then
    Exit;
  if FActiveBlock <> nil then
    CommitActiveBlock;
  if (FMaxBlockCount > 0) and (FBlocks.Count >= FMaxBlockCount) then
  begin
    ABlock.Free;
    raise ERangeError.Create(SCnMarkDownTooManyBlocks);
  end;
  ABlock.FBlockID := FNextBlockID;
  Inc(FNextBlockID);
  ABlock.FRevision := 1;
  ABlock.FStable := True;
  Index := FBlocks.Add(ABlock);
  Changed(cmdBlockAdded, Index);
  Changed(cmdBlockCommitted, Index);
end;

constructor TCnMarkDownStreamParser.Create;
begin
  inherited Create;
  FDocument := TCnMarkDownDocument.Create;
  FLineBuffer := TCnMarkDownTextBuffer.Create;
  FCurrentBuffer := TCnMarkDownTextBuffer.Create;
  FMaxSourceLength := CN_MARKDOWN_DEFAULT_MAX_SOURCE_LENGTH;
  FMaxChunkLength := CN_MARKDOWN_DEFAULT_MAX_CHUNK_LENGTH;
  FMaxLineLength := CN_MARKDOWN_DEFAULT_MAX_LINE_LENGTH;
  FMaxBlockLength := CN_MARKDOWN_DEFAULT_MAX_BLOCK_LENGTH;
  FBuildInlines := True;
  Reset;
end;

destructor TCnMarkDownStreamParser.Destroy;
begin
  FCurrentBuffer.Free;
  FLineBuffer.Free;
  FDocument.Free;
  inherited Destroy;
end;

function TCnMarkDownStreamParser.GetMaxBlockCount: Integer;
begin
  Result := FDocument.MaxBlockCount;
end;

procedure TCnMarkDownStreamParser.SetMaxBlockCount(Value: Integer);
begin
  FDocument.MaxBlockCount := Value;
end;

procedure TCnMarkDownStreamParser.CheckBlockLength(Value: Int64);
begin
  if (FMaxBlockLength > 0) and (Value > FMaxBlockLength) then
    raise ERangeError.Create(SCnMarkDownBlockTooLong);
end;

procedure TCnMarkDownStreamParser.ValidateChunk(
  const AChunk: TCnMarkDownText);
var
  I, LineLength: Integer;
  C: WideChar;
  PendingCR: Boolean;
begin
  if (FMaxChunkLength > 0) and (Length(AChunk) > FMaxChunkLength) then
    raise ERangeError.Create(SCnMarkDownChunkTooLong);
  if (FMaxSourceLength > 0) and
    (Int64(Length(AChunk)) > FMaxSourceLength - FScanOffset) then
    raise ERangeError.Create(SCnMarkDownSourceTooLong);
  if FMaxLineLength <= 0 then
    Exit;
  LineLength := FLineBuffer.Length;
  PendingCR := FPendingCR;
  for I := 1 to Length(AChunk) do
  begin
    C := AChunk[I];
    if PendingCR then
    begin
      LineLength := 0;
      PendingCR := False;
      if C = #10 then
        Continue;
    end;
    if C = #13 then
      PendingCR := True
    else if C = #10 then
      LineLength := 0
    else
    begin
      Inc(LineLength);
      if LineLength > FMaxLineLength then
        raise ERangeError.Create(SCnMarkDownLineTooLong);
    end;
  end;
end;

procedure TCnMarkDownStreamParser.Reset;
begin
  FDocument.Clear;
  FLineBuffer.Clear;
  FCurrentBuffer.Clear;
  FCurrentBlockType := cmbUnknown;
  FCurrentSourceStart := 0;
  FCurrentSourceEnd := 0;
  FCurrentQuoteLevel := 0;
  FCurrentLineCount := 0;
  FCodeLanguage := '';
  FInFence := False;
  FFenceChar := #0;
  FFenceLength := 0;
  FPendingCR := False;
  FScanOffset := 0;
  FLineStartOffset := 0;
  FFinished := False;
end;

procedure TCnMarkDownStreamParser.CommitCurrentBlock;
var
  Block: TCnMarkDownBlock;
begin
  if FCurrentBlockType = cmbUnknown then
    Exit;
  Block := TCnMarkDownBlock.Create;
  Block.BlockType := FCurrentBlockType;
  Block.Text := FCurrentBuffer.Text;
  Block.SourceStart := FCurrentSourceStart;
  Block.SourceLength := FCurrentSourceEnd - FCurrentSourceStart;
  Block.QuoteLevel := FCurrentQuoteLevel;
  Block.CodeLanguage := FCodeLanguage;
  if FBuildInlines then
    Block.RebuildInlines;
  FDocument.UpdateActiveBlock(Block);
  FDocument.CommitActiveBlock;
  FCurrentBuffer.Clear;
  FCurrentBlockType := cmbUnknown;
  FCurrentSourceStart := FCurrentSourceEnd;
  FCurrentQuoteLevel := 0;
  FCurrentLineCount := 0;
  FCodeLanguage := '';
end;

procedure TCnMarkDownStreamParser.RefreshActiveBlock;
var
  Block: TCnMarkDownBlock;
  Buffer: TCnMarkDownTextBuffer;
  FenceChar: WideChar;
  FenceLength: Integer;
  Language: TCnMarkDownText;
begin
  if FCurrentBlockType <> cmbUnknown then
  begin
    Buffer := TCnMarkDownTextBuffer.Create;
    try
      Buffer.Append(FCurrentBuffer.Text);
      if FLineBuffer.Length > 0 then
      begin
        if FCurrentLineCount > 0 then
          Buffer.AppendChar(#10);
        Buffer.Append(FLineBuffer.Text);
      end;
      CheckBlockLength(Buffer.Length);
      Block := TCnMarkDownBlock.Create;
      Block.BlockType := FCurrentBlockType;
      Block.Text := Buffer.Text;
      Block.SourceStart := FCurrentSourceStart;
      Block.SourceLength := FScanOffset - FCurrentSourceStart;
      Block.QuoteLevel := FCurrentQuoteLevel;
      Block.CodeLanguage := FCodeLanguage;
      if FBuildInlines then
        Block.RebuildInlines;
      FDocument.UpdateActiveBlock(Block);
    finally
      Buffer.Free;
    end;
    Exit;
  end;

  if (FLineBuffer.Length = 0) or CnMarkDownTextIsBlank(FLineBuffer.Text) then
    Exit;
  Block := CnMarkDownCreateLineBlock(FLineBuffer.Text,
    FLineStartOffset, FScanOffset - FLineStartOffset);
  if not FBuildInlines then
    Block.ClearInlines;
  try
    CheckBlockLength(Length(Block.Text));
  except
    Block.Free;
    raise;
  end;
  FenceChar := #0;
  FenceLength := 0;
  Language := '';
  if CnMarkDownTryFence(FLineBuffer.Text, FenceChar, FenceLength,
    Language) then
  begin
    Block.BlockType := cmbCodeBlock;
    Block.Text := '';
    Block.CodeLanguage := Language;
    if FBuildInlines then
      Block.RebuildInlines;
  end;
  FDocument.UpdateActiveBlock(Block);
end;

procedure TCnMarkDownStreamParser.ProcessLine(
  const Line: TCnMarkDownText; ASourceStart, ASourceLength: Int64);
var
  Block: TCnMarkDownBlock;
  FenceChar: WideChar;
  FenceLength: Integer;
  Language: TCnMarkDownText;
begin
  if FInFence then
  begin
    if CnMarkDownIsFenceClose(Line, FFenceChar, FFenceLength) then
    begin
      FCurrentSourceEnd := ASourceStart + ASourceLength;
      CommitCurrentBlock;
      FInFence := False;
      FFenceChar := #0;
      FFenceLength := 0;
    end
    else
    begin
      if FCurrentLineCount > 0 then
        CheckBlockLength(Int64(FCurrentBuffer.Length) + 1 + Length(Line))
      else
        CheckBlockLength(Length(Line));
      if FCurrentLineCount > 0 then
        FCurrentBuffer.AppendChar(#10);
      FCurrentBuffer.Append(Line);
      Inc(FCurrentLineCount);
      FCurrentSourceEnd := ASourceStart + ASourceLength;
    end;
    Exit;
  end;

  if CnMarkDownTextIsBlank(Line) then
  begin
    CommitCurrentBlock;
    Exit;
  end;

  FenceChar := #0;
  FenceLength := 0;
  Language := '';
  if CnMarkDownTryFence(Line, FenceChar, FenceLength, Language) then
  begin
    CommitCurrentBlock;
    FCurrentBlockType := cmbCodeBlock;
    FCurrentBuffer.Clear;
    FCurrentSourceStart := ASourceStart;
    FCurrentSourceEnd := ASourceStart + ASourceLength;
    FCurrentQuoteLevel := 0;
    FCurrentLineCount := 0;
    FCodeLanguage := Language;
    FInFence := True;
    FFenceChar := FenceChar;
    FFenceLength := FenceLength;
    Exit;
  end;

  Block := CnMarkDownCreateLineBlock(Line, ASourceStart, ASourceLength);
  if not FBuildInlines then
    Block.ClearInlines;
  try
    CheckBlockLength(Length(Block.Text));
  except
    Block.Free;
    raise;
  end;
  if Block.BlockType = cmbParagraph then
  begin
    if (FCurrentBlockType = cmbParagraph) and
      (FCurrentQuoteLevel = Block.QuoteLevel) then
    begin
      try
        CheckBlockLength(Int64(FCurrentBuffer.Length) + 1 +
          Length(Block.Text));
      except
        Block.Free;
        raise;
      end;
      if FCurrentLineCount > 0 then
        FCurrentBuffer.AppendChar(#10);
      FCurrentBuffer.Append(Block.Text);
      Inc(FCurrentLineCount);
      FCurrentSourceEnd := ASourceStart + ASourceLength;
      Block.Free;
    end
    else
    begin
      CommitCurrentBlock;
      FCurrentBlockType := cmbParagraph;
      FCurrentBuffer.Clear;
      FCurrentBuffer.Append(Block.Text);
      FCurrentSourceStart := ASourceStart;
      FCurrentSourceEnd := ASourceStart + ASourceLength;
      FCurrentQuoteLevel := Block.QuoteLevel;
      FCurrentLineCount := 1;
      Block.Free;
    end;
  end
  else
  begin
    CommitCurrentBlock;
    FDocument.UpdateActiveBlock(Block);
    FDocument.CommitActiveBlock;
  end;
end;

procedure TCnMarkDownStreamParser.Append(const AChunk: TCnMarkDownText);
var
  I: Integer;
  C: WideChar;
  Line: TCnMarkDownText;
begin
  if FFinished or (AChunk = '') then
    Exit;
  ValidateChunk(AChunk);
  FDocument.AppendSource(AChunk);
  I := 1;
  while I <= Length(AChunk) do
  begin
    C := AChunk[I];
    if FPendingCR then
    begin
      if C = #10 then
      begin
        Inc(FScanOffset);
        Line := FLineBuffer.Text;
        FLineBuffer.Clear;
        ProcessLine(Line, FLineStartOffset,
          FScanOffset - FLineStartOffset);
        FLineStartOffset := FScanOffset;
        FPendingCR := False;
        Inc(I);
        Continue;
      end
      else
      begin
        Line := FLineBuffer.Text;
        FLineBuffer.Clear;
        ProcessLine(Line, FLineStartOffset,
          FScanOffset - FLineStartOffset);
        FLineStartOffset := FScanOffset;
        FPendingCR := False;
      end;
    end;

    if C = #13 then
    begin
      FPendingCR := True;
      Inc(FScanOffset);
    end
    else if C = #10 then
    begin
      Inc(FScanOffset);
      Line := FLineBuffer.Text;
      FLineBuffer.Clear;
      ProcessLine(Line, FLineStartOffset,
        FScanOffset - FLineStartOffset);
      FLineStartOffset := FScanOffset;
    end
    else
    begin
      FLineBuffer.AppendChar(C);
      Inc(FScanOffset);
    end;
    Inc(I);
  end;
  RefreshActiveBlock;
end;

procedure TCnMarkDownStreamParser.Finish;
var
  Line: TCnMarkDownText;
begin
  if FFinished then
    Exit;
  if FPendingCR then
  begin
    Line := FLineBuffer.Text;
    FLineBuffer.Clear;
    ProcessLine(Line, FLineStartOffset, FScanOffset - FLineStartOffset);
    FLineStartOffset := FScanOffset;
    FPendingCR := False;
  end
  else if FLineBuffer.Length > 0 then
  begin
    Line := FLineBuffer.Text;
    FLineBuffer.Clear;
    ProcessLine(Line, FLineStartOffset, FScanOffset - FLineStartOffset);
    FLineStartOffset := FScanOffset;
  end;
  CommitCurrentBlock;
  FInFence := False;
  FFenceChar := #0;
  FFenceLength := 0;
  FFinished := True;
  FLineBuffer.ReleaseStorage;
  FCurrentBuffer.ReleaseStorage;
  FDocument.Changed(cmdFinished, -1);
end;

function IsBlank(const Str: string): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  I: Integer;
begin
  for I := 1 to Length(Str) do
  begin
    if Str[I] <> ' ' then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

function IsCRLF(C: Char): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (C = #13) or (C = #10);
end;

function IsSpaceOrTab(C: Char): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (C = ' ') or (C = #9);
end;

{ TCnMarkDownParser }

procedure TCnMarkDownParser.BraceProc;
begin
  StepRun;
  FTokenID := cmtLink;

  while not (FOrigin[FRun] in [')', #0]) do
    StepRun;

  if FOrigin[FRun] = ')' then
    StepRun;
end;

constructor TCnMarkDownParser.Create;
begin
  inherited;
end;

destructor TCnMarkDownParser.Destroy;
begin

  inherited;
end;

procedure TCnMarkDownParser.ExclamationProc;
begin
  StepRun;
  if FOrigin[FRun] = '[' then
    FTokenID := cmtImageSign
  else
    FTokenID := cmtContent;
end;

function TCnMarkDownParser.GetToken: string;
var
  Len: Cardinal;
  OutStr: string;
begin
  Len := FRun - FTokenPos;                         // 两个偏移量之差，单位为字符数
  SetString(OutStr, (FOrigin + FTokenPos), Len);   // 以指定内存地址与字符长度构造字符串
  Result := OutStr;
end;

function TCnMarkDownParser.GetTokenLength: Integer;
begin
  Result := FRun - FTokenPos;
end;

procedure TCnMarkDownParser.GreaterHeaderProc;
begin
  StepRun;
  FTokenID := cmtContent;

  if IsSpaceOrTab(FOrigin[FRun]) then
  begin
    FTokenID := cmtQuota;
    StepRun;
    FIsLineStart := True; // 注意！引用符号后，仍需当成行首解析后续内容，因此手工设置
  end;
end;

procedure TCnMarkDownParser.LessProc;
begin
  StepRun;
  FTokenID := cmtDirectLink;

  while not (FOrigin[FRun] in ['>', #0]) do
    StepRun;

  if FOrigin[FRun] = '>' then
    StepRun;
end;

procedure TCnMarkDownParser.LineBreakProc;
begin
  FTokenID := cmtContent;
  while FOrigin[FRun] = #13 do
    StepRun;

  if FOrigin[FRun] = #10 then
  begin
    FTokenID := cmtLineBreak;
    StepRun;
  end;
end;

procedure TCnMarkDownParser.LoadFromBookmark(var Bookmark: TCnMarkDownBookmark);
begin
  FRun := Bookmark.Run;
  FTokenPos := Bookmark.TokenPos;
  FIsLineStart := Bookmark.IsLineStart;
  FTokenID := Bookmark.TokenID;
end;

procedure TCnMarkDownParser.MinusHeaderProc;
begin
  StepRun;
  FTokenID := cmtContent;

  if FOrigin[FRun] = '-' then             // 独立三个凑一个分隔线
  begin
    StepRun;
    if FOrigin[FRun] = '-' then
    begin
      StepRun;
      if IsCRLF(FOrigin[FRun]) then
      begin
        FTokenID := cmtLine;
        StepRun;
      end;
    end;
  end
  else if IsSpaceOrTab(FOrigin[FRun]) then // 行首单个凑无序列表
  begin
    FTokenID := cmtUnOrderedList;
    StepRun;
  end;
end;

procedure TCnMarkDownParser.Next;

  // 从当前字符往后跑到敏感字符为止，注意无论 Ansi 还是 Utf8 还是 Utf16 都应有效
  procedure StepTo;
  begin
    repeat
      StepRun;

      if FIsLineStart then
      begin
        // 行首的话，这些字符要跳出
        if FOrigin[FRun] in ['#', '<', '>', '0'..'9', '+', '-', '(', '!',
          '_', '*', '`', '[', ' ', #9, #13, #10] then
          Exit;
      end
      else
      begin
        // 非行首的话，这些字符要跳出，本来空格也要的，但为了效率，单个不跳
        if FOrigin[FRun] in ['<', '(', '*', '`', '~', '_', '!', '[', #13, #10] then
          Exit
        else if (FOrigin[FRun] = ' ') and (FOrigin[FRun + 1] = ' ') then
          Exit;
      end;
    until FOrigin[FRun] = #0;
  end;

begin
  FTokenPos := FRun;

  if FIsLineStart then
  begin
    // 以下判断行首有效
    case FOrigin[FRun] of
      '#':
        SharpHeaderProc;
      '>':
        GreaterHeaderProc;
      '0'..'9':
        NumberHeaderProc;
      '+':
        PlusHeaderProc;
      '-':
        MinusHeaderProc;
      '_':
        UnderLineProc;
      '*':
        StarProc;
      '`':
        QuotaProc;
      ' ':
        SpaceProc;
      #9:
        TabHeaderProc;
      #13, #10:
        LineBreakProc;
      #0:
        TerminateProc;
    else
      FTokenID := cmtContent;
      StepTo;
    end;
  end
  else // 以下非行首也有效，内部要根据行首进行判断
  begin
    case FOrigin[FRun] of
      '*':
        StarProc;
      '`':
        QuotaProc;
      '~':
        WaveProc;
      '[':
        SquareProc;
      '<':
        LessProc;
      '(':
        BraceProc;
      '!':
        ExclamationProc;
      '_':
        UnderLineProc;
      ' ':
        SpaceProc;
      #13, #10:
        LineBreakProc;
      #0:
        TerminateProc;
    else
      FTokenID := cmtContent;
      StepTo;
    end;
  end;
end;

procedure TCnMarkDownParser.NumberHeaderProc;
begin
  StepRun;
  FTokenID := cmtContent;

  if FOrigin[FRun] = '.' then
  begin
    StepRun;
    if IsSpaceOrTab(FOrigin[FRun]) then
    begin
      FTokenID := cmtOrderedList;
      StepRun;
    end;
  end
  else if FOrigin[FRun] in ['0'..'9'] then
  begin
    StepRun;
    if FOrigin[FRun] = '.' then
    begin
      StepRun;
      if IsSpaceOrTab(FOrigin[FRun]) then
      begin
        FTokenID := cmtOrderedList;
        StepRun;
      end;
    end;
  end;
end;

procedure TCnMarkDownParser.PlusHeaderProc;
begin
  StepRun;
  FTokenID := cmtContent;

  if FOrigin[FRun] = '+' then
  begin
    StepRun;
    if FOrigin[FRun] = '+' then
    begin
      StepRun;
      if IsCRLF(FOrigin[FRun]) then
      begin
        FTokenID := cmtLine;
        StepRun;
      end;
    end;
  end;
end;

procedure TCnMarkDownParser.QuotaProc;
var
  IsLS: Boolean;
begin
  IsLS := FIsLineStart;
  StepRun;

  if IsLS then
  begin
    FTokenID := cmtContent;

    if FOrigin[FRun] = '`' then
    begin
      StepRun;
      if FOrigin[FRun] = '`' then
      begin
        FTokenID := cmtFenceCodeBlock;
        StepRun;
      end;
    end;
  end
  else
  begin
    FTokenID := cmtCodeBlock;
  end;
end;

procedure TCnMarkDownParser.SaveToBookmark(var Bookmark: TCnMarkDownBookmark);
begin
  Bookmark.Run := FRun;
  Bookmark.TokenPos := FTokenPos;
  Bookmark.IsLineStart := FIsLineStart;
  Bookmark.TokenID := FTokenID;
end;

procedure TCnMarkDownParser.SetOrigin(const Value: PChar);
begin
  FOrigin := Value;
  FRun := 0;
  FIsLineStart := True;
  Next;
end;

procedure TCnMarkDownParser.SharpHeaderProc;
begin
  StepRun;
  FTokenID := cmtContent;

  if IsSpaceOrTab(FOrigin[FRun]) then
  begin
    FTokenID := cmtHeading1;
    StepRun;
  end
  else if FOrigin[FRun] = '#' then
  begin
    StepRun;
    if IsSpaceOrTab(FOrigin[FRun]) then
    begin
      FTokenID := cmtHeading2;
      StepRun;
    end
    else if FOrigin[FRun] = '#' then
    begin
      StepRun;
      if IsSpaceOrTab(FOrigin[FRun]) then
      begin
        FTokenID := cmtHeading3;
        StepRun;
      end
      else if FOrigin[FRun] = '#' then
      begin
        StepRun;
        if IsSpaceOrTab(FOrigin[FRun]) then
        begin
          FTokenID := cmtHeading4;
          StepRun;
        end
        else if FOrigin[FRun] = '#' then
        begin
          StepRun;
          if IsSpaceOrTab(FOrigin[FRun]) then
          begin
            FTokenID := cmtHeading5;
            StepRun;
          end
          else if FOrigin[FRun] = '#' then
          begin
            StepRun;
            if IsSpaceOrTab(FOrigin[FRun]) then
            begin
              FTokenID := cmtHeading6;
              StepRun;
            end
            else if FOrigin[FRun] = '#' then
            begin
              FTokenID := cmtHeading7;
              StepRun;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TCnMarkDownParser.SpaceProc;
var
  IsLS: Boolean;
  Bookmark: TCnMarkDownBookmark;
begin
  IsLS := FIsLineStart;
  StepRun;
  FTokenID := cmtSpace;

  if IsLS then                // 行首四个空格做缩进
  begin
    if FOrigin[FRun] = ' ' then
    begin
      StepRun;
      if FOrigin[FRun] = ' ' then
      begin
        StepRun;
        if FOrigin[FRun] = ' ' then
        begin
          FTokenID := cmtIndent;
          StepRun;
        end;
      end;
    end;
  end
  else // 平时俩空格加（回车）换行
  begin
    SaveToBookmark(Bookmark);
    if FOrigin[FRun] = ' ' then
    begin
      StepRun;
      if FOrigin[FRun] = #10 then
      begin
        FTokenID := cmtHardBreak;
        StepRun;
      end
      else if (FOrigin[FRun] = #13) and (FOrigin[FRun + 1] = #10) then
      begin
        FTokenID := cmtHardBreak;
        StepRun;
        StepRun;
      end
      else
        LoadFromBookmark(Bookmark); // 不是硬回车，需要回到起始空格处
    end
    else
      LoadFromBookmark(Bookmark);   // 不是硬回车，需要回到起始空格处
  end;

  if IsLS and (FTokenID = cmtSpace) then // 行首的空格越过后，仍然当行首计算
    FIsLineStart := True;
end;

procedure TCnMarkDownParser.SquareProc;
begin
  StepRun;
  FTokenID := cmtLinkDisplay;

  while not (FOrigin[FRun] in [']', #0]) do
    StepRun;

  if FOrigin[FRun] = ']' then
    StepRun;
end;

procedure TCnMarkDownParser.StarProc;
var
  IsLS: Boolean;
begin
  IsLS := FIsLineStart;
  StepRun;
  FTokenID := cmtContent;

  if IsLS then
  begin
    if IsSpaceOrTab(FOrigin[FRun]) then // 行首的*空格代表无序列表
    begin
      FTokenID := cmtUnOrderedList;
      StepRun;
    end
    else if FOrigin[FRun] = '*' then
    begin
      StepRun;
      if FOrigin[FRun] = '*' then
      begin
        StepRun;
        if IsCRLF(FOrigin[FRun]) then
        begin
          FTokenID := cmtLine;  // 行首三个星号换行算线
          StepRun;
        end
        else
        begin
          // 行首三个星号后代表粗斜体，上面已经越过了
          FTokenID := cmtBoldItalic;
        end;
      end
      else
      begin
        // 两个星号，上面已经越过了
        FTokenID := cmtBold;
      end;
    end
    else
    begin
      // 行首的单个星号代表斜体，开头已经越过了
      FTokenID := cmtItalic;
    end;
  end
  else
  begin
    if FOrigin[FRun] = '*' then
    begin
      StepRun;
      if FOrigin[FRun] = '*' then
      begin
        // 三个星号代表粗斜体
        FTokenID := cmtBoldItalic;
        StepRun;
      end
      else
      begin
        // 两个星号，上面已经越过了
        FTokenID := cmtBold;
      end;
    end
    else
    begin
      // 单个星号，开头已经越过了
      FTokenID := cmtItalic;
    end;
  end;
end;

procedure TCnMarkDownParser.StepRun;
var
  IsLF: Boolean;
begin
  IsLF := FOrigin[FRun] = #10;
  Inc(FRun);
  FIsLineStart := IsLF and (FOrigin[FRun] <> #13) and (FOrigin[FRun] <> #10);
end;

procedure TCnMarkDownParser.TabHeaderProc;
begin
  StepRun;
  FTokenID := cmtIndent;
end;

procedure TCnMarkDownParser.TerminateProc;
begin
  FTokenID := cmtTerminate;
end;

procedure TCnMarkDownParser.UnderLineProc;
var
  IsLS: Boolean;
begin
  IsLS := FIsLineStart;
  StepRun;
  FTokenID := cmtContent;

  if IsLS then
  begin
    if FOrigin[FRun] = '_' then
    begin
      StepRun;
      if FOrigin[FRun] = '_' then
      begin
        StepRun;
        if IsCRLF(FOrigin[FRun]) then
        begin
          FTokenID := cmtLine;  // 行首三个下划线换行算线
          StepRun;
        end
        else
        begin
          // 行首三个下划线算粗斜体，上面已经越过了
          FTokenID := cmtBoldItalic;
        end;
      end
      else
      begin
        // 行首两个下划线算粗体，上面已经越过了
        FTokenID := cmtBold;
      end;
    end
    else
    begin
      // 行首单个下划线算斜体，开头已经越过了
      FTokenID := cmtItalic;
    end;
  end
  else
  begin
    if FOrigin[FRun] = '_' then
    begin
      StepRun;
      if FOrigin[FRun] = '_' then
      begin
        // 三个下划线算粗斜体
        FTokenID := cmtBoldItalic;
        StepRun;
      end
      else
      begin
        // 两个下划线算粗体，上面越过了
        FTokenID := cmtBold;
      end;
    end
    else
    begin
      // 单个下划线算斜体，开头已经越过了
      FTokenID := cmtItalic;
    end;
  end;
end;

procedure TCnMarkDownParser.WaveProc;
begin
  StepRun;
  if FOrigin[FRun] = '~' then  // 两个连续的 ~ 是删除线
  begin
    FTokenID := cmtStroke;
    StepRun;
  end
  else
    FTokenID := cmtContent;
end;

{ TCnMarkDownBase }

function TCnMarkDownBase.Add(AMarkDown: TCnMarkDownBase): TCnmarkDownBase;
begin
  FItems.Add(AMarkDown);
  AMarkDown.Parent := Self;
  Result := AMarkDown;
end;

constructor TCnMarkDownBase.Create;
begin
  inherited;
  FItems := TObjectList.Create(True);
end;

procedure TCnMarkDownBase.Delete(Index: Integer);
begin
  FItems.Delete(Index);
end;

destructor TCnMarkDownBase.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TCnMarkDownBase.Extract(Index: Integer): TCnMarkDownBase;
begin
  Result := TCnMarkDownBase(FItems.Extract(FItems[Index]));
end;

function TCnMarkDownBase.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TCnMarkDownBase.GetItem(Index: Integer): TCnMarkDownBase;
begin
  Result := TCnMarkDownBase(FItems.Items[Index]);
end;

procedure TCnMarkDownBase.SetItem(Index: Integer;
  const Value: TCnMarkDownBase);
begin
  FItems.Items[Index] := Value;
end;

procedure CnMarkDownDebugOutputLevel(MarkDown: TCnMarkDownBase; List: TStrings; Level: Integer = 0);
var
  I: Integer;
  S, IndentStr: string;
  Para: TCnMarkDownParagraph;
  Fragment: TCnMarkDownTextFragment;
  TypeName: string;
begin
  if (MarkDown = nil) or (List = nil) then
    Exit;

  // 生成缩进字符串（每级缩进 4 个空格）
  IndentStr := StringOfChar(' ', Level * 4);

  // 根据节点类型输出信息
  if MarkDown is TCnMarkDownParagraph then
  begin
    Para := TCnMarkDownParagraph(MarkDown);
    // 获取段落类型枚举名称
    TypeName := GetEnumName(TypeInfo(TCnMarkDownParagraphType), Ord(Para.ParagraphType));
    S := IndentStr + '[Paragraph] ' + TypeName;
    if Para.CodeType <> '' then
      S := S + ': ' + Para.CodeType;
    List.Add(S);
  end
  else if MarkDown is TCnMarkDownTextFragment then
  begin
    Fragment := TCnMarkDownTextFragment(MarkDown);
    // 获取片段类型枚举名称
    TypeName := GetEnumName(TypeInfo(TCnMarkDownTextFragmentType), Ord(Fragment.FragmentType));
    List.Add(IndentStr + '[Fragment] ' + TypeName + ' Length: ' + IntToStr(Length(Fragment.Content))
      + ' - ' + Fragment.Content);
  end
  else
  begin
    // 未知节点类型
    List.Add(IndentStr + '[Node] ' + MarkDown.ClassName);
  end;

  // 递归处理子节点
  for I := 0 to MarkDown.Count - 1 do
    CnMarkDownDebugOutputLevel(MarkDown.Items[I], List, Level + 1);
end;

procedure CnMarkDownDebugOutput(MarkDown: TCnMarkDownBase; List: TStrings);
begin
  CnMarkDownDebugOutputLevel(MarkDown, List, 0);
end;

function TokenTypeToParaType(TokenType: TCnMarkDownTokenType): TCnMarkDownParagraphType;
begin
  case TokenType of
    cmtHeading1: Result := cmpHeading1;
    cmtHeading2: Result := cmpHeading2;
    cmtHeading3: Result := cmpHeading3;
    cmtHeading4: Result := cmpHeading4;
    cmtHeading5: Result := cmpHeading5;
    cmtHeading6: Result := cmpHeading6;
    cmtHeading7: Result := cmpHeading7;

    cmtLine: Result := cmpLine;
    cmtIndent: Result := cmpPre;
    cmtQuota: Result := cmpQuota;
    cmtFenceCodeBlock: Result := cmpFenceCodeBlock;

    cmtUnOrderedList: Result := cmpUnorderedList;
    cmtOrderedList: Result := cmpOrderedList;
    cmtLineBreak: Result := cmpEmpty; // 未被处理的独立空行

    cmtContent,
    cmtCodeBlock,      // `
    cmtBold,           // ** 或 __
    cmtItalic,         // * 或 _
    cmtBoldItalic,     // *** 或 ___
    cmtStroke,         // ~~
    cmtLinkDisplay,    // [
    cmtLink,           // (
    cmtDirectLink,     // <...> 中的内容
    cmtImageSign:      // ! 后面必须紧跟 [
      Result := cmpCommon; // 行内格式和普通内容，都是普通段落

    // TODO: 其他
  else
    Result := cmpUnknown;
  end;
end;

procedure ParseMarkDownToLineEnd(P: TCnMarkDownParser; Parent: TCnMarkDownParagraph);
var
  Frag: TCnMarkDownTextFragment;
  PT: TCnMarkDownParagraphType;
  Bookmark: TCnMarkDownBookmark;

  function MapTokenToBrace(ATokenType: TCnMarkDownTokenType): TCnMarkDownTextBraceType;
  begin
    case ATokenType of
      cmtBold:       Result := cmtbBold;
      cmtItalic:     Result := cmtbItalic;
      cmtBoldItalic: Result := cmtbBoldItalic;
      cmtStroke:     Result := cmtbStroke;
      cmtCodeBlock:  Result := cmtbCodeBlock;
    else
      Result := cmtbNone;
    end;
  end;

  // 注意该函数返回 True 时，ParentLastOpenFrag 须返回对应 Open 的 Fragment
  function ParentFragmentHasLastOpenToken(AnOpen: TCnMarkDownTokenType): Boolean;
  var
    F: TCnMarkDownTextFragment;
    B: TCnMarkDownTextBraceType;
  begin
    // 从后往前找 Parent 的 Fragment 里是否有开放的
    // cmtBold, cmtItalic, cmtBoldItalic, cmtStroke, cmtCodeBlock 等
    // 以决定本次遇到的是开还是闭，注意处理了交叉
    B := MapTokenToBrace(AnOpen);
    if Parent.Count > 0 then
    begin
      F := TCnMarkDownTextFragment(Parent.Items[Parent.Count - 1]);
      if (F.FragmentType in CN_MARKDOWN_FRAGMENTTYPE_NEED_MATCH)
        and (F.OpenType = B) and (F.CloseType <> B) then
      begin
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  end;

  // 最近的一个应关闭而未关闭的 Fragment
  // 注意它和 ParentFragmentHasLastOpenToken 判断的依据必须相同
  function ParentLastOpenFrag: TCnMarkDownTextFragment;
  var
    F: TCnMarkDownTextFragment;
  begin
    Result := nil;
    if Parent.Count > 0 then
    begin
      F := TCnMarkDownTextFragment(Parent.Items[Parent.Count - 1]);
      if (F.FragmentType in CN_MARKDOWN_FRAGMENTTYPE_NEED_MATCH)          // 应关闭而未关闭的
        and (F.CloseType = cmtbNone) then
      begin
        Result := F;
        Exit;
      end;

      if F.FragmentType in [cmfDirectLink, cmfLink, cmfLinkDisplay] then // 直接链等几个是单块，不可再加东西
        Result := nil;
    end;
  end;

  // 最近一个可加东西的，不能是闭合的配对块，不能是单块
  function ParentLastCommonFrag: TCnMarkDownTextFragment;
  var
    F: TCnMarkDownTextFragment;
  begin
    Result := nil;
    if Parent.Count > 0 then
    begin
      F := TCnMarkDownTextFragment(Parent.Items[Parent.Count - 1]);
      if F.FragmentType in [cmfDirectLink, cmfLink, cmfLinkDisplay] then // 不能是单块
        Exit;

      if (F.FragmentType in CN_MARKDOWN_FRAGMENTTYPE_NEED_MATCH) // 不能是闭合的配对块
        and (F.CloseType <> cmtbNone) then
        Exit;

      if F.FragmentType = cmfCommon then
        Result := F;
    end;
  end;

  procedure AddCommonContent(const Str: string);
  var
    L: TCnMarkDownTextFragment;
  begin
    L := ParentLastOpenFrag;
    if L = nil then // 上一个独立、完备
    begin
      L := ParentLastCommonFrag;
      if L = nil then // 或者没有上一个，就加个新的
      begin
        Frag := TCnMarkDownTextFragment.Create;
        Frag.FragmentType := cmfCommon;

        Parent.Add(Frag);
        Frag.AddContent(Str);
        Exit;
      end;
    end;
    L.AddContent(Str);
  end;

begin
  // 解析直到行尾的内容然后添加到 Parent 下，并越过行尾的换行指向下一个。供调用者 Next
  // 进来时，P 在前一个 Token，这里按需 Next
  // 结束分两种情况：
  // 一、普通换行或硬换行后强行结束（比如 Parent 是 Heading），不管下一行是啥
  // 二、普通换行后看自己以及下一行是啥决定是否结束（比如自己是普通段落，普通换行则不结束得连续俩换行，或硬换行结束）

  PT := Parent.ParagraphType;
  if PT = cmpPre then
  begin
    // Pre 要原封不动处理单行
    Frag := TCnMarkDownTextFragment.Create;
    Frag.FragmentType := cmfCommon;
    Parent.Add(Frag);

    repeat
      P.Next;
      Frag.AddContent(P.Token);
    until (P.TokenID in [cmtTerminate, cmtLineBreak, cmtHardBreak]); // 普通回车或硬回车结束
  end
  else if PT = cmpFenceCodeBlock then
  begin
    // 对 ``` 的处理是起始，因为独立的 ``` 行结束在外层调用者处会判断处理
    if P.TokenID = cmtFenceCodeBlock then
    begin
      // Start 后一个 Content 当代码类型名
      repeat
        P.Next;
        if (P.TokenID = cmtContent) and (Parent.CodeType = '') then
          Parent.CodeType := P.Token;
      until (P.TokenID in [cmtTerminate, cmtLineBreak, cmtHardBreak]); // 普通回车或硬回车结束
      P.Next; // 越过回车
    end
    else // 非 Fence，直接记录
    begin
      Frag := TCnMarkDownTextFragment.Create;
      Frag.FragmentType := cmfCommon;
      Parent.Add(Frag);
      Frag.AddContent(P.Token);

      repeat
        P.Next;
        Frag.AddContent(P.Token);
      until (P.TokenID in [cmtTerminate, cmtLineBreak, cmtHardBreak]); // 普通回车或硬回车结束
      P.Next; // 越过回车
    end;
  end
  else
  begin
    if PT in [cmpHeading1..cmpHeading7, cmpOrderedList, cmpUnOrderedList, cmpLine] then // 这几个段落有开始标记，跳过
      P.Next;

    // 循环解析行内容
    while P.TokenID <> cmtTerminate do
    begin
      // CodeBlock 里无需解析
      if (P.TokenID <> cmtCodeBlock) and ParentFragmentHasLastOpenToken(cmtCodeBlock) then
        AddCommonContent(P.Token)
      else
      begin
        case P.TokenID of
          cmtHardBreak:
            begin
              // 记录当前 Frag 为硬回车，外头会中断段落
              Frag := TCnMarkDownTextFragment.Create;
              Frag.FragmentType := cmfHardBreak;

              Parent.Add(Frag);
              Break;
            end;
          cmtLineBreak:
            begin
              // 要确保退出循环时 P.TokenID 指向换行
              if PT in [cmpHeading1..cmpHeading7, cmpOrderedList, cmpUnOrderedList] then // 这些单个就退出
                Break;

              P.SaveToBookmark(Bookmark);
              P.Next;

              // 连续两个也退出，一些典型段落开头也退出但要回退到换行，其他普通内容继续
              if P.TokenID = cmtLineBreak then
                Break
              else if P.TokenID in CN_MARKDOWN_TOKENTYPE_PARAHEAD then
              begin
                // 回退到上一 Token
                P.LoadFromBookmark(Bookmark);
                Break;
              end
              else if P.TokenID = cmtContent then
                AddCommonContent(P.Token);
            end;
          cmtBold:
            begin
              if ParentFragmentHasLastOpenToken(cmtBold) then
                ParentLastOpenFrag.CloseType := cmtbBold
              else
              begin
                Frag := TCnMarkDownTextFragment.Create;
                Frag.FragmentType := cmfBold;
                Frag.OpenType := cmtbBold;

                Parent.Add(Frag);
              end;
            end;
          cmtItalic:
            begin
              if ParentFragmentHasLastOpenToken(cmtItalic) then
                ParentLastOpenFrag.CloseType := cmtbItalic
              else
              begin
                Frag := TCnMarkDownTextFragment.Create;
                Frag.FragmentType := cmfItalic;
                Frag.OpenType := cmtbItalic;

                Parent.Add(Frag);
              end;
            end;
          cmtBoldItalic:
            begin
              if ParentFragmentHasLastOpenToken(cmtBoldItalic) then
                ParentLastOpenFrag.CloseType := cmtbBoldItalic
              else
              begin
                Frag := TCnMarkDownTextFragment.Create;
                Frag.FragmentType := cmfBoldItalic;
                Frag.OpenType := cmtbBoldItalic;

                Parent.Add(Frag);
              end;
            end;
          cmtStroke:
            begin
              if ParentFragmentHasLastOpenToken(cmtStroke) then
                ParentLastOpenFrag.CloseType := cmtbStroke
              else
              begin
                Frag := TCnMarkDownTextFragment.Create;
                Frag.FragmentType := cmfStroke;
                Frag.OpenType := cmtbStroke;

                Parent.Add(Frag);
              end;
            end;
          cmtCodeBlock:
            begin
              if ParentFragmentHasLastOpenToken(cmtCodeBlock) then
                ParentLastOpenFrag.CloseType := cmtbCodeBlock
              else
              begin
                Frag := TCnMarkDownTextFragment.Create;
                Frag.FragmentType := cmfCodeBlock;
                Frag.OpenType := cmtbCodeBlock;

                Parent.Add(Frag);
              end;
            end;
          cmtLinkDisplay:
            begin
              Frag := TCnMarkDownTextFragment.Create;
              Frag.FragmentType := cmfLinkDisplay;
              Frag.AddContent(P.Token);
              Parent.Add(Frag);
            end;
          cmtLink:
            begin
              Frag := TCnMarkDownTextFragment.Create;
              Frag.FragmentType := cmfLink;
              Frag.AddContent(P.Token);
              Parent.Add(Frag);
            end;
          cmtDirectLink:
            begin
              Frag := TCnMarkDownTextFragment.Create;
              Frag.FragmentType := cmfDirectLink;
              Frag.AddContent(P.Token);
              Parent.Add(Frag);
            end;
          cmtImageSign:
            begin
              Frag := TCnMarkDownTextFragment.Create;
              Frag.FragmentType := cmfImage;
              Frag.AddContent(P.Token);
              Parent.Add(Frag);
            end;
        else
          AddCommonContent(P.Token);
        end;
      end;
      P.Next;
    end;
  end;
end;

function CnParseMarkDownString(const MarkDown: string): TCnMarkDownBase;
var
  I, J: Integer;
  P: TCnMarkDownParser;
  Root: TCnMarkDownBase;
  CurPara, P2, P1: TCnMarkDownParagraph;
  F: TCnMarkDownTextFragment;
  ParaStack: TStack;

  procedure NewParagraph;
  var
    Para: TCnMarkDownParagraph;
  begin
    Para := TCnMarkDownParagraph.Create;
    Para.ParagraphType := TokenTypeToParaType(P.TokenID);
    if CurPara = nil then
    begin
      Root.Add(Para);
      ParaStack.Push(Root);
    end
    else
    begin
      CurPara.Add(Para);
      ParaStack.Push(CurPara);
    end;

    CurPara := Para;
  end;

  procedure EndParagraph;
  begin
    // 普通段落结束后，引用的标记要全部弹出清理掉，其他情况只要弹出
    while ParaStack.Count > 0 do
    begin
      CurPara := TCnMarkDownParagraph(ParaStack.Pop);
      if CurPara.ParagraphType <> cmpQuota then
        Break;
    end;
  end;

begin
  Root := TCnMarkDownBase.Create; // 作为 Root

  P := nil;
  ParaStack := nil;

  try
    P := TCnMarkDownParser.Create;
    try
      P.SetOrigin(PChar(MarkDown));
      ParaStack := TStack.Create;
      CurPara := nil;

      while P.TokenID <> cmtTerminate do
      begin
        // 这里要确保每个 case 都是段落开始
        case P.TokenID of
          cmtHeading1..cmtHeading7:
            begin
              // 创建新段落并设置标题级别
              NewParagraph;
              ParseMarkDownToLineEnd(P, CurPara);
              EndParagraph;
            end;
          cmtLine:
            begin
              // 线段
              NewParagraph;
              P.Next; // 直接越过线段，让循环越过后面的换行
              EndParagraph;
            end;
          cmtUnOrderedList:
            begin
              // 无序列表的每一条
              NewParagraph;
              ParseMarkDownToLineEnd(P, CurPara);
              EndParagraph;
            end;
          cmtOrderedList:
            begin
              // 有序列表的每一条
              NewParagraph;
              ParseMarkDownToLineEnd(P, CurPara);
              EndParagraph;
            end;
          cmtFenceCodeBlock:
            begin
              // 代码大块是一段
              NewParagraph;
              repeat
                ParseMarkDownToLineEnd(P, CurPara);
              until P.TokenID in [cmtFenceCodeBlock, cmtTerminate];
              EndParagraph;
            end;
          cmtIndent:
            begin
              // 缩进原始格式块
              NewParagraph;
              ParseMarkDownToLineEnd(P, CurPara);
              EndParagraph;
            end;
          cmtQuota:
            begin
              // 引用块，块后是新的一段
              NewParagraph;
            end;
        else // cmtContent, cmtLinkDisplay, cmtDirectLink, cmtImageSign:
          // 其他普通内容，跳过开始的空格，解析整行
          if not IsBlank(P.Token) then
          begin
            NewParagraph;
            ParseMarkDownToLineEnd(P, CurPara);
            EndParagraph;
          end;
        end;

        P.Next;
      end;
    finally
      ParaStack.Free;
      P.Free;
    end;
  except
    Root.Free; // 解析途中如有异常则释放 Root
    Root := nil;
    raise;
  end;

  if Root <> nil then
  begin
    // 将硬回车分开的连续普通段拼在一起
    if Root.Count >= 2 then
    begin
      for I := Root.Count - 1 downto 1 do
      begin
        if (Root[I] is TCnMarkDownParagraph) and (Root[I - 1] is TCnMarkDownParagraph) then
        begin
          P2 := TCnMarkDownParagraph(Root[I]);
          P1 := TCnMarkDownParagraph(Root[I - 1]);
          if (P2.ParagraphType = cmpCommon) and (P1.ParagraphType = cmpCommon) then
          begin
            // 如果 P1 的最后一个 Fragment 是硬回车
            if (P1.Count > 0) and (P1[P1.Count - 1] is TCnMarkDownTextFragment) then
            begin
              F := TCnMarkDownTextFragment(P1[P1.Count - 1]);
              if F.FragmentType = cmfHardBreak then
              begin
                // 把 P2 的内容从 0 开始 Extract 出来加给 P1
                for J := 0 to P2.Count - 1 do
                  P1.Add(P2.Extract(0));

                Root.Delete(I); // 删除并释放 P2
              end;
            end;
          end;
        end;
      end;
    end;

    // 再清除掉无用的空段
    for I := Root.Count - 1 downto 0 do
    begin
      if Root[I] is TCnMarkDownParagraph then
      begin
        if (TCnMarkDownParagraph(Root[I]).Count = 0) and
          (TCnMarkDownParagraph(Root[I]).ParagraphType = cmpEmpty) then
          Root.Delete(I);
      end;
    end;
  end;
  Result := Root;
end;

{ TCnMarkDownTextFragment }

procedure TCnMarkDownTextFragment.AddContent(const Cont: string);
begin
  FContent := FContent + Cont;
end;

function TCnMarkDownTextFragment.GetContent: string;
begin
  Result := FContent;
end;

procedure CnMarkDownRTFAppend(Builder: TCnStringBuilder;
  const Value: AnsiString);
begin
{$IFDEF UNICODE}
  Builder.AppendAnsi(Value);
{$ELSE}
  Builder.Append(string(Value));
{$ENDIF}
end;

procedure CnMarkDownWriteUnicodeRTFText(Builder: TCnStringBuilder;
  const Text: TCnMarkDownText);
var
  I, N: Integer;
  C: WideChar;
  S: AnsiString;
begin
  for I := 1 to Length(Text) do
  begin
    C := Text[I];
    case C of
      '\': CnMarkDownRTFAppend(Builder, '\\');
      '{': CnMarkDownRTFAppend(Builder, '\{');
      '}': CnMarkDownRTFAppend(Builder, '\}');
      #9: CnMarkDownRTFAppend(Builder, '\tab ');
      #10: CnMarkDownRTFAppend(Builder, '\line ');
      #13: ;
    else
      N := Ord(C);
      if (N >= 32) and (N <= 126) then
      begin
        SetLength(S, 1);
        S[1] := AnsiChar(N);
        CnMarkDownRTFAppend(Builder, S);
      end
      else
      begin
        if N > 32767 then
          Dec(N, 65536);
        CnMarkDownRTFAppend(Builder,
          AnsiString('\u' + IntToStr(N) + '?'));
      end;
    end;
  end;
end;

procedure CnMarkDownWriteUnicodeRTFColor(Builder: TCnStringBuilder;
  Color: Cardinal);
begin
  CnMarkDownRTFAppend(Builder, '\red' +
    AnsiString(IntToStr((Color shr 16) and $FF)) + '\green' +
    AnsiString(IntToStr((Color shr 8) and $FF)) + '\blue' +
    AnsiString(IntToStr(Color and $FF)) + ';');
end;

procedure CnMarkDownWriteUnicodeRTFHeader(Builder: TCnStringBuilder;
  ABasicFontSize: Integer; AStyle: TCnMarkDownRTFStyle);
var
  FontName, CodeFontName: TCnMarkDownText;
  TextColor, LinkColor, CodeBackgroundColor, SecondaryColor: Cardinal;
begin
  if ABasicFontSize < 1 then
    ABasicFontSize := 1;
  if AStyle <> nil then
  begin
    FontName := AStyle.FontName;
    CodeFontName := AStyle.CodeFontName;
    TextColor := AStyle.TextColor;
    LinkColor := AStyle.LinkColor;
    CodeBackgroundColor := AStyle.CodeBackgroundColor;
    SecondaryColor := AStyle.SecondaryColor;
  end
  else
  begin
    FontName := 'Segoe UI';
    CodeFontName := 'Consolas';
    TextColor := $000000;
    LinkColor := $0066CC;
    CodeBackgroundColor := $F0F0F0;
    SecondaryColor := $606060;
  end;
  CnMarkDownRTFAppend(Builder,
    '{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fnil ');
  CnMarkDownWriteUnicodeRTFText(Builder, FontName);
  CnMarkDownRTFAppend(Builder, ';}{\f1\fmodern ');
  CnMarkDownWriteUnicodeRTFText(Builder, CodeFontName);
  CnMarkDownRTFAppend(Builder, ';}}{\colortbl;');
  CnMarkDownWriteUnicodeRTFColor(Builder, TextColor);
  CnMarkDownWriteUnicodeRTFColor(Builder, LinkColor);
  CnMarkDownWriteUnicodeRTFColor(Builder, CodeBackgroundColor);
  CnMarkDownWriteUnicodeRTFColor(Builder, SecondaryColor);
  CnMarkDownRTFAppend(Builder, '}\viewkind4\uc1\pard\cf1\f0\fs' +
    AnsiString(IntToStr(ABasicFontSize * 2)) + ' ');
end;

procedure CnMarkDownWriteUnicodeRTFInline(Builder: TCnStringBuilder;
  AInline: TCnMarkDownInline);
begin
  case AInline.InlineType of
    cmiStrong:
      CnMarkDownRTFAppend(Builder, '{\b ');
    cmiEmphasis:
      CnMarkDownRTFAppend(Builder, '{\i ');
    cmiStrongEmphasis:
      CnMarkDownRTFAppend(Builder, '{\b\i ');
    cmiStrikethrough:
      CnMarkDownRTFAppend(Builder, '{\strike ');
    cmiCode:
      CnMarkDownRTFAppend(Builder, '{\f1\highlight3 ');
    cmiLink:
      CnMarkDownRTFAppend(Builder, '{\cf2\ul ');
    cmiImage:
      CnMarkDownRTFAppend(Builder, '{\i\cf4 ');
    cmiSoftBreak:
      begin
        CnMarkDownRTFAppend(Builder, ' ');
        Exit;
      end;
    cmiHardBreak:
      begin
        CnMarkDownRTFAppend(Builder, '\line ');
        Exit;
      end;
  end;
  CnMarkDownWriteUnicodeRTFText(Builder, AInline.Text);
  if AInline.InlineType <> cmiText then
    CnMarkDownRTFAppend(Builder, '}');
end;

procedure CnMarkDownWriteUnicodeRTFBlock(Builder: TCnStringBuilder;
  Block: TCnMarkDownBlock; ABasicFontSize: Integer);
var
  I, FontSize, LeftIndent: Integer;
begin
  if (Builder = nil) or (Block = nil) then
    Exit;
  if ABasicFontSize < 1 then
    ABasicFontSize := 1;
  LeftIndent := Block.QuoteLevel * 360;
  case Block.BlockType of
    cmbHeading:
      begin
        FontSize := ABasicFontSize * 2 +
          (7 - Block.HeadingLevel) * 2;
        if FontSize < ABasicFontSize * 2 then
          FontSize := ABasicFontSize * 2;
        CnMarkDownRTFAppend(Builder, '{\pard\sa120\li' +
          AnsiString(IntToStr(LeftIndent)) + '\b\fs' +
          AnsiString(IntToStr(FontSize)) + ' ');
      end;
    cmbThematicBreak:
      begin
        CnMarkDownRTFAppend(Builder,
          '{\pard\sa120\brdrb\brdrs\brdrw15\par}');
        Exit;
      end;
    cmbCodeBlock:
      CnMarkDownRTFAppend(Builder, '{\pard\li' +
        AnsiString(IntToStr(LeftIndent + 240)) +
        '\ri120\sa120\f1\highlight3 ');
    cmbUnorderedListItem:
      CnMarkDownRTFAppend(Builder, '{\pard\li' +
        AnsiString(IntToStr(LeftIndent + Block.ListLevel * 720)) +
        '\fi-360\sa60 \u8226?\tab ');
    cmbOrderedListItem:
      CnMarkDownRTFAppend(Builder, '{\pard\li' +
        AnsiString(IntToStr(LeftIndent + Block.ListLevel * 720)) +
        '\fi-360\sa60 ' + AnsiString(IntToStr(Block.ListStart)) +
        '.\tab ');
  else
    CnMarkDownRTFAppend(Builder, '{\pard\li' +
      AnsiString(IntToStr(LeftIndent)) + '\sa90 ');
  end;

  if (Block.BlockType = cmbCodeBlock) or (Block.InlineCount = 0) then
    CnMarkDownWriteUnicodeRTFText(Builder, Block.Text)
  else
    for I := 0 to Block.InlineCount - 1 do
      CnMarkDownWriteUnicodeRTFInline(Builder, Block.Inlines[I]);
  CnMarkDownRTFAppend(Builder, '\par}');
end;

function CnMarkDownBlockToUnicodeRTF(Block: TCnMarkDownBlock;
  ABasicFontSize: Integer; IncludeHeader: Boolean;
  AStyle: TCnMarkDownRTFStyle): AnsiString;
var
  Builder: TCnStringBuilder;
begin
  Builder := TCnStringBuilder.Create(True);
  try
    if IncludeHeader then
      CnMarkDownWriteUnicodeRTFHeader(Builder, ABasicFontSize, AStyle);
    CnMarkDownWriteUnicodeRTFBlock(Builder, Block, ABasicFontSize);
    if IncludeHeader then
      CnMarkDownRTFAppend(Builder, '}');
    Result := Builder.ToAnsiString;
  finally
    Builder.Free;
  end;
end;

function CnMarkDownDocumentToUnicodeRTF(Document: TCnMarkDownDocument;
  ABasicFontSize: Integer; AStyle: TCnMarkDownRTFStyle): AnsiString;
var
  I: Integer;
  Builder: TCnStringBuilder;
begin
  Builder := TCnStringBuilder.Create(True);
  try
    CnMarkDownWriteUnicodeRTFHeader(Builder, ABasicFontSize, AStyle);
    if Document <> nil then
      for I := 0 to Document.BlockCount - 1 do
        CnMarkDownWriteUnicodeRTFBlock(Builder, Document.Blocks[I],
          ABasicFontSize);
    CnMarkDownRTFAppend(Builder, '}');
    Result := Builder.ToAnsiString;
  finally
    Builder.Free;
  end;
end;

procedure CnMarkDownWriteRTFStream(AStream: TStream;
  const Value: AnsiString);
begin
  if (AStream <> nil) and (Value <> '') then
    AStream.WriteBuffer(Value[1], Length(Value));
end;

procedure CnMarkDownBlockToUnicodeRTFStream(AStream: TStream;
  Block: TCnMarkDownBlock; ABasicFontSize: Integer;
  IncludeHeader: Boolean; AStyle: TCnMarkDownRTFStyle);
begin
  if AStream = nil then
    Exit;
  CnMarkDownWriteRTFStream(AStream,
    CnMarkDownBlockToUnicodeRTF(Block, ABasicFontSize, IncludeHeader,
      AStyle));
end;

procedure CnMarkDownDocumentToUnicodeRTFStream(AStream: TStream;
  Document: TCnMarkDownDocument; ABasicFontSize: Integer;
  AStyle: TCnMarkDownRTFStyle);
var
  I: Integer;
  Builder: TCnStringBuilder;
begin
  if AStream = nil then
    Exit;
  Builder := TCnStringBuilder.Create(True);
  try
    CnMarkDownWriteUnicodeRTFHeader(Builder, ABasicFontSize, AStyle);
    CnMarkDownWriteRTFStream(AStream, Builder.ToAnsiString);
  finally
    Builder.Free;
  end;
  if Document <> nil then
    for I := 0 to Document.BlockCount - 1 do
      CnMarkDownWriteRTFStream(AStream,
        CnMarkDownBlockToUnicodeRTF(Document.Blocks[I],
          ABasicFontSize, False, AStyle));
  CnMarkDownWriteRTFStream(AStream, '}');
end;

{ TCnRTFConverter }

function TCnRTFConverter.Convert(Root: TCnMarkDownBase): string;
var
  I: Integer;
begin
  FRtf.Clear;
  FRtf.Append(Format(CN_RTF_HEADER, [PointToHalfPoint(BasicFontSize)]));

  for I := 0 to Root.Count - 1 do
    ProcessNode(Root.Items[I]);

  FRtf.Append(CN_RTF_FOOTER);
  Result := FRtf.ToString;
end;

function TCnRTFConverter.ConvertFragment(Fragment: TCnMarkDownTextFragment): string;
begin
  case Fragment.FragmentType of
    cmfCodeBlock:   Result := '\f3\highlight3\b ' + EscapeContent(Fragment.Content) + '\b0\highlight0\f0 ';
    cmfBold:        Result := '\b ' + EscapeContent(Fragment.Content) + '\b0 ';
    cmfItalic:      Result := '\i ' + EscapeContent(Fragment.Content) + '\i0 ';
    cmfBoldItalic:  Result := '\b\i ' + EscapeContent(Fragment.Content) + '\i0\b0 ';
    cmfStroke:      Result := '\strike ' + EscapeContent(Fragment.Content) + '\strike0 ';
    cmfLink:        Result := '\cf2 ' + EscapeContent(Fragment.Content) + '\cf0 ';
    cmfHardBreak:   Result := '\line ';
  else
    Result := EscapeContent(Fragment.Content);
  end;
end;

function TCnRTFConverter.ConvertParagraphEnd(Paragraph: TCnMarkDownParagraph): string;
begin
  if Paragraph.ParagraphType = cmpLine then
    Result := #13#10 // 分隔线无需补充尾部内容
  else if Paragraph.ParagraphType <> cmpQuota then // Quota 也不是具体段落，不写
    Result :='\par}'#13#10;
end;

function TCnRTFConverter.ConvertParagraphStart(Paragraph: TCnMarkDownParagraph): string;
var
  L, Q: Integer;

  function HeadSizeFactor(Head: TCnMarkDownParagraphType): Extended;
  begin
    Result := 1.0;
    case Head of
      cmpHeading1: Result := 1.5;
      cmpHeading2: Result := 1.4;
      cmpHeading3: Result := 1.3;
      cmpHeading4: Result := 1.2;
      cmpHeading5: Result := 1.1;
      cmpHeading6: Result := 1.0;
      cmpHeading7: Result := 0.9;
    end;
  end;

  function TwipsFromFontSizeFactor(F: Extended): Integer;
  begin
    Result := Round(PointToTwips(BasicFontSize) * F);
  end;

begin
  Q := GetQuotaLevel(Paragraph);
  case Paragraph.ParagraphType of
    cmpHeading1..cmpHeading7:
      begin
        L := Round(PointToHalfPoint(BasicFontSize) * HeadSizeFactor(Paragraph.ParagraphType));
        Result := Format('{\pard\fs%d\sa%d\b ', [L, PointToTwips(Round(L * 0.3))]);
        // 标题的段后间距少一点点
      end;
    cmpUnorderedList:
      begin
        Result := Format('{\pard{\pntext\f2\''B7\tab}{\*\pn\pnlvlblt\pnf2\pnindent0{\pntxtb\''B7}}\fi%d\li%d\sa%d\sl276\slmult1 ',
          [CN_LIST_UNINDENT, CN_LIST_INDENT + Q * CN_QUOTA_INDENT, TwipsFromFontSizeFactor(0.5)]);
      end;
    cmpOrderedList:
      begin
        L := GetListLevel(Paragraph);  // 列表缩进层级
        Q := GetQuotaLevel(Paragraph); // 引用缩进层级
        Inc(FListCounters[L]);         // 列表序号

        Result := Format('{\pard{\pntext\f0 %d.\tab}\fi%d\li%d\sa%d\sl276\slmult1\lang2052\f0\fs%d ',
          [FListCounters[L], CN_LIST_UNINDENT, CN_LIST_INDENT + Q * CN_QUOTA_INDENT,
           TwipsFromFontSizeFactor(0.5), PointToHalfPoint(BasicFontSize)]);
      end;
    cmpLine:
      Result := Format('{\pard\sa%d\brdrb\brdrs\brdrw15\par}', [TwipsFromFontSizeFactor(0.5)]);
    cmpQuota:
      begin
        // Quota 本身不是具体段落，不写
      end;
    cmpFenceCodeBlock:
      Result := Format('{\pard\f3\fs%d\cbpat3\brdrs\brdrw15\brdrcf3\box\sa30 ', [PointToHalfPoint(BasicFontSize)]);
  else
    // 普通内层段落缩进由引用层级控制，并保持一定段后间距，稍微大一点点
    if (Paragraph.Parent <> nil) and (Paragraph.Parent is TCnMarkDownParagraph) then
      Result := Format('{\pard\li%d\sa%d ', [Q * CN_QUOTA_INDENT, TwipsFromFontSizeFactor(0.6)])
    else
      Result := Format('{\pard\li0\fi0\sa%d ', [TwipsFromFontSizeFactor(0.6)]); // 顶级段落用默认缩进 0 的格式
  end;
end;

constructor TCnRTFConverter.Create;
begin
  inherited;
  FRtf := TCnStringBuilder.Create;
end;

destructor TCnRTFConverter.Destroy;
begin
  FRtf.Free;
  inherited;
end;

function TCnRTFConverter.EscapeContent(const Text: string): string;
{$IFDEF UNICODE}
var
  Builder: TCnStringBuilder;
{$ENDIF}
begin
{$IFDEF UNICODE}
  Builder := TCnStringBuilder.Create(True);
  try
    CnMarkDownWriteUnicodeRTFText(Builder, Text);
    Result := string(Builder.ToAnsiString);
  finally
    Builder.Free;
  end;
{$ELSE}
  Result := StringReplace(Text, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '{', '\{', [rfReplaceAll]);
  Result := StringReplace(Result, '}', '\}', [rfReplaceAll]);
  Result := StringReplace(Result, #13#10, '\line ', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\line ', [rfReplaceAll]);
{$ENDIF}
end;

function TCnRTFConverter.GetListLevel(Paragraph: TCnMarkDownParagraph): Integer;
var
  Node: TCnMarkDownBase;
begin
  Result := 1;
  Node := Paragraph.Parent;
  while (Node <> nil) and (Node is TCnMarkDownParagraph) do
  begin
    if TCnMarkDownParagraph(Node).ParagraphType in [cmpOrderedList, cmpUnorderedList] then
      Inc(Result);
    Node := Node.Parent;
  end;
end;

function TCnRTFConverter.GetQuotaLevel(Paragraph: TCnMarkDownParagraph): Integer;
var
  Node: TCnMarkDownBase;
begin
  Result := 0;
  if Paragraph.ParagraphType = cmpQuota then
    Inc(Result);

  Node := Paragraph.Parent;
  while (Node <> nil) and (Node is TCnMarkDownParagraph) do
  begin
    if TCnMarkDownParagraph(Node).ParagraphType in [cmpQuota] then
      Inc(Result);
    Node := Node.Parent;
  end;
end;

function TCnRTFConverter.PointToHalfPoint(Point: Integer): Integer;
begin
  Result := Point shl 1;
end;

function TCnRTFConverter.PointToTwips(Point: Integer): Integer;
begin
  Result := Point * 20;
end;

procedure TCnRTFConverter.ProcessNode(Node: TCnMarkDownBase);
var
  I: Integer;
begin
  if Node is TCnMarkDownParagraph then
    FRtf.Append(ConvertParagraphStart(TCnMarkDownParagraph(Node)));

  for I := 0 to Node.Count - 1 do
    ProcessNode(Node.Items[I]);

  if Node is TCnMarkDownParagraph then
    FRtf.Append(ConvertParagraphEnd(TCnMarkDownParagraph(Node)))
  else if Node is TCnMarkDownTextFragment then
    FRtf.Append(ConvertFragment(TCnMarkDownTextFragment(Node)));
end;

function CnMarkDownConvertToRTF(Root: TCnMarkDownBase; ABasicFontSize: Integer): string;
begin
  with TCnRTFConverter.Create do
  try
    BasicFontSize := ABasicFontSize;
    Result := Convert(Root);
  finally
    Free;
  end;
end;

{ TCnMarkDownConverter }

constructor TCnMarkDownConverter.Create;
begin
  FBasicFontSize := 12; // 默认 12 Point
end;

destructor TCnMarkDownConverter.Destroy;
begin

  inherited;
end;

{ TCnVirtualHeightIndex }

constructor TCnVirtualHeightIndex.Create;
begin
  inherited Create;
  Clear;
end;

procedure TCnVirtualHeightIndex.Clear;
begin
  SetLength(FHeights, 0);
  SetLength(FTree, 0);
  FCount := 0;
  FTotal := 0;
end;

procedure TCnVirtualHeightIndex.AddTree(Index, Delta: Integer);
var
  I: Integer;
begin
  I := Index + 1;
  while I <= FCount do
  begin
    FTree[I] := FTree[I] + Delta;
    I := I + (I and -I);
  end;
end;

procedure TCnVirtualHeightIndex.SetCount(ACount, ADefaultHeight: Integer);
var
  I: Integer;
begin
  if ACount < 0 then
    ACount := 0;
  if ADefaultHeight < 1 then
    ADefaultHeight := 1;

  SetLength(FHeights, ACount);
  SetLength(FTree, ACount + 1);
  FCount := ACount;
  FTotal := 0;
  for I := 0 to FCount - 1 do
  begin
    FHeights[I] := ADefaultHeight;
    FTotal := FTotal + ADefaultHeight;
  end;
  for I := 1 to FCount do
    FTree[I] := ADefaultHeight;
  for I := 1 to FCount do
    if I + (I and -I) <= FCount then
      FTree[I + (I and -I)] := FTree[I + (I and -I)] + FTree[I];
end;

procedure TCnVirtualHeightIndex.Append(AHeight: Integer);
begin
  if AHeight < 1 then
    AHeight := 1;
  SetLength(FHeights, FCount + 1);
  SetLength(FTree, FCount + 2);
  Inc(FCount);
  FHeights[FCount - 1] := AHeight;
  FTotal := FTotal + AHeight;
  AddTree(FCount - 1, AHeight);
end;

function TCnVirtualHeightIndex.GetHeight(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FHeights[Index]
  else
    Result := 0;
end;

procedure TCnVirtualHeightIndex.SetHeight(Index, Value: Integer);
var
  Delta: Integer;
begin
  if (Index < 0) or (Index >= FCount) then
    Exit;
  if Value < 1 then
    Value := 1;
  Delta := Value - FHeights[Index];
  if Delta = 0 then
    Exit;
  FHeights[Index] := Value;
  FTotal := FTotal + Delta;
  AddTree(Index, Delta);
end;

function TCnVirtualHeightIndex.PrefixHeight(ACount: Integer): Int64;
var
  I: Integer;
begin
  if ACount < 0 then
    ACount := 0;
  if ACount > FCount then
    ACount := FCount;
  Result := 0;
  I := ACount;
  while I > 0 do
  begin
    Result := Result + FTree[I];
    I := I - (I and -I);
  end;
end;

function TCnVirtualHeightIndex.TopOf(Index: Integer): Int64;
begin
  if Index <= 0 then
    Result := 0
  else if Index >= FCount then
    Result := FTotal
  else
    Result := PrefixHeight(Index);
end;

function TCnVirtualHeightIndex.BottomOf(Index: Integer): Int64;
begin
  if (Index < 0) or (Index >= FCount) then
    Result := 0
  else
    Result := PrefixHeight(Index + 1);
end;

function TCnVirtualHeightIndex.IndexAtOffset(AOffset: Int64): Integer;
var
  Step, Node, NextNode: Integer;
  Prefix: Int64;
begin
  if FCount = 0 then
  begin
    Result := -1;
    Exit;
  end;
  if AOffset < 0 then
    AOffset := 0;
  if AOffset >= FTotal then
    AOffset := FTotal - 1;

  Node := 0;
  Prefix := 0;
  Step := 1;
  while Step < FCount do
    Step := Step shl 1;
  while Step > 0 do
  begin
    NextNode := Node + Step;
    if (NextNode <= FCount) and
      (Prefix + FTree[NextNode] <= AOffset) then
    begin
      Node := NextNode;
      Prefix := Prefix + FTree[NextNode];
    end;
    Step := Step shr 1;
  end;
  Result := Node;
  if Result >= FCount then
    Result := FCount - 1;
end;

{ TCnMarkDownFeedMessage }

constructor TCnMarkDownFeedMessage.Create(AID: Int64;
  ARole: TCnMarkDownMessageRole);
begin
  inherited Create;
  FID := AID;
  FRole := ARole;
  FParser := TCnMarkDownStreamParser.Create;
  FParser.BuildInlines := False;
  FParser.Document.KeepSource := False;
  FPending := TCnMarkDownTextBuffer.Create;
end;

destructor TCnMarkDownFeedMessage.Destroy;
begin
  FParser.Free;
  FPending.Free;
  inherited Destroy;
end;

function TCnMarkDownFeedMessage.GetDocument: TCnMarkDownDocument;
begin
  Result := FParser.Document;
end;

function TCnMarkDownFeedMessage.GetFinished: Boolean;
begin
  Result := FParser.Finished;
end;

function TCnMarkDownFeedMessage.GetKeepSource: Boolean;
begin
  Result := FParser.Document.KeepSource;
end;

function TCnMarkDownFeedMessage.GetPendingLength: Integer;
begin
  Result := FPending.Length;
end;

procedure TCnMarkDownFeedMessage.SetKeepSource(Value: Boolean);
begin
  FParser.Document.KeepSource := Value;
end;

procedure TCnMarkDownFeedMessage.Append(const AChunk: TCnMarkDownText);
begin
  if FPending.Length > 0 then
    FlushQueued(0);
  FParser.Append(AChunk);
end;

procedure TCnMarkDownFeedMessage.Queue(const AChunk: TCnMarkDownText);
begin
  FPending.Append(AChunk);
end;

function TCnMarkDownFeedMessage.FlushQueued(AMaxChars: Integer): Integer;
var
  Chunk: TCnMarkDownText;
begin
  if AMaxChars <= 0 then
    AMaxChars := FPending.Length;
  if AMaxChars > FPending.Length then
    AMaxChars := FPending.Length;
  if AMaxChars <= 0 then
  begin
    Result := 0;
    Exit;
  end;
  Chunk := FPending.ExtractPrefix(AMaxChars);
  FParser.Append(Chunk);
  Result := AMaxChars;
end;

procedure TCnMarkDownFeedMessage.Finish;
begin
  FlushQueued(0);
  FParser.Finish;
end;

procedure TCnMarkDownFeedMessage.Reset;
begin
  FPending.Clear;
  FParser.Reset;
end;

{ TCnMarkDownFeed }

constructor TCnMarkDownFeed.Create;
begin
  inherited Create;
  FMessages := TObjectList.Create(True);
  FNextID := 1;
  FMaxMessageCount := 100000;
  FKeepSourceText := False;
end;

destructor TCnMarkDownFeed.Destroy;
begin
  FMessages.Free;
  inherited Destroy;
end;

function TCnMarkDownFeed.GetMessage(Index: Integer): TCnMarkDownFeedMessage;
begin
  Result := TCnMarkDownFeedMessage(FMessages[Index]);
end;

function TCnMarkDownFeed.GetMessageCount: Integer;
begin
  Result := FMessages.Count;
end;

procedure TCnMarkDownFeed.Changed(ChangeType: TCnMarkDownFeedChangeType;
  MessageIndex: Integer);
begin
  if FUpdateCount > 0 then
    Exit;
  if Assigned(FOnChange) then
    FOnChange(Self, ChangeType, MessageIndex);
end;

procedure TCnMarkDownFeed.Clear;
begin
  FMessages.Clear;
  FNextID := 1;
  Changed(cmfReset, -1);
end;

function TCnMarkDownFeed.AddMessage(
  ARole: TCnMarkDownMessageRole): TCnMarkDownFeedMessage;
begin
  if (FMaxMessageCount > 0) and (FMessages.Count >= FMaxMessageCount) then
    raise ERangeError.Create('The Markdown feed exceeds the message limit.');
  Result := TCnMarkDownFeedMessage.Create(FNextID, ARole);
  Result.KeepSource := FKeepSourceText;
  Inc(FNextID);
  FMessages.Add(Result);
  Changed(cmfMessageAdded, FMessages.Count - 1);
end;

procedure TCnMarkDownFeed.AppendMessage(MessageIndex: Integer;
  const AChunk: TCnMarkDownText);
begin
  if (MessageIndex < 0) or (MessageIndex >= FMessages.Count) then
    raise ERangeError.Create('The Markdown message index is invalid.');
  Messages[MessageIndex].Append(AChunk);
  Changed(cmfMessageChanged, MessageIndex);
end;

procedure TCnMarkDownFeed.QueueMessage(MessageIndex: Integer;
  const AChunk: TCnMarkDownText);
begin
  if (MessageIndex < 0) or (MessageIndex >= FMessages.Count) then
    raise ERangeError.Create('The Markdown message index is invalid.');
  Messages[MessageIndex].Queue(AChunk);
  Changed(cmfMessageQueued, MessageIndex);
end;

function TCnMarkDownFeed.FlushMessageQueue(MessageIndex,
  AMaxChars: Integer): Integer;
begin
  if (MessageIndex < 0) or (MessageIndex >= FMessages.Count) then
    raise ERangeError.Create('The Markdown message index is invalid.');
  Result := Messages[MessageIndex].FlushQueued(AMaxChars);
  if Result > 0 then
    Changed(cmfMessageChanged, MessageIndex);
end;

procedure TCnMarkDownFeed.FinishMessage(MessageIndex: Integer);
begin
  if (MessageIndex < 0) or (MessageIndex >= FMessages.Count) then
    raise ERangeError.Create('The Markdown message index is invalid.');
  Messages[MessageIndex].Finish;
  Changed(cmfMessageFinished, MessageIndex);
end;

procedure TCnMarkDownFeed.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCnMarkDownFeed.EndUpdate;
begin
  if FUpdateCount = 0 then
    Exit;
  Dec(FUpdateCount);
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self, cmfBatchChanged, -1);
end;

end.
