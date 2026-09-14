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
*           已完成的消息也允许继续增量追加，便于局部内容更新后重新排版。
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
  Classes, SysUtils, Contnrs, CnStrings;

type
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
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AppendBlockText(BlockIndex: Integer;
      const AChunk: TCnMarkDownText);
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
    procedure AppendToBlock(BlockIndex: Integer;
      const AChunk: TCnMarkDownText);
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
    procedure AppendToBlock(MessageIndex, BlockIndex: Integer;
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

procedure TCnMarkDownDocument.AppendBlockText(BlockIndex: Integer;
  const AChunk: TCnMarkDownText);
var
  Block: TCnMarkDownBlock;
begin
  if AChunk = '' then
    Exit;
  if FKeepSource then
    raise EInvalidOperation.Create(
      'Appending directly to a parsed block requires KeepSource=False.');
  if FActiveBlock <> nil then
    raise EInvalidOperation.Create(
      'Appending directly to a block requires a finished message.');
  if (BlockIndex < 0) or (BlockIndex >= FBlocks.Count) then
    raise ERangeError.Create('The Markdown block index is invalid.');
  Block := TCnMarkDownBlock(FBlocks[BlockIndex]);
  if Length(AChunk) > MaxInt - Length(Block.Text) then
    raise ERangeError.Create(SCnMarkDownBlockTooLong);
  Block.Text := Block.Text + AChunk;
  Block.SourceLength := Block.SourceLength + Length(AChunk);
  Block.RebuildInlines;
  Inc(Block.FRevision);
  { 直接修改已完成块后仍保持稳定状态，只递增修订号通知虚拟视图重排。 }
  Changed(cmdBlockChanged, BlockIndex);
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
  { 完成状态只表示当前批次已结束；再次追加时从文档尾部继续解析。 }
  if AChunk = '' then
    Exit;
  ValidateChunk(AChunk);
  if FFinished then
    FFinished := False;
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

procedure TCnMarkDownFeedMessage.AppendToBlock(BlockIndex: Integer;
  const AChunk: TCnMarkDownText);
begin
  if not FParser.Finished then
    raise EInvalidOperation.Create(
      'Appending directly to a block requires a finished message.');
  if FPending.Length > 0 then
    raise EInvalidOperation.Create(
      'The message still has pending stream data.');
  if (BlockIndex < 0) or
    (BlockIndex >= FParser.Document.BlockCount) then
    raise ERangeError.Create('The Markdown block index is invalid.');
  if (FParser.MaxBlockLength > 0) and
    (Int64(Length(AChunk)) +
      Int64(Length(FParser.Document.Blocks[BlockIndex].Text)) >
      FParser.MaxBlockLength) then
    raise ERangeError.Create(SCnMarkDownBlockTooLong);
  FParser.Document.AppendBlockText(BlockIndex, AChunk);
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

procedure TCnMarkDownFeed.AppendToBlock(MessageIndex, BlockIndex: Integer;
  const AChunk: TCnMarkDownText);
begin
  if (MessageIndex < 0) or (MessageIndex >= FMessages.Count) then
    raise ERangeError.Create('The Markdown message index is invalid.');
  Messages[MessageIndex].AppendToBlock(BlockIndex, AChunk);
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
