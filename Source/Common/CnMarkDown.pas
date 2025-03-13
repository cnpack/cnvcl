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
* 开发平台：PWin7 + Delphi 5
* 兼容测试：PWin7 + Delphi 2009 ~
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2025.03.06 V1.0
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
    procedure TerminateProc; // #0

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

    property Origin: PAnsiChar read FOrigin write SetOrigin;
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

  TCnMarkDownTextFragmentType = (cmfUnknown, cmfCommon, cmfBold, cmfItalic,
    cmfBoldItalic, cmfStroke, cmfCodeBlock, cmfLink, cmfLinkDisplay, cmfImage, cmfDirectLink);
  {* 文本类型}
  TCnMarkDownTextFragmentTypes = set of TCnMarkDownTextFragmentType;

  TCnMarkDownTextBraceType = (cmtbNone, cmtbBold, cmtbItalic, cmtbBoldItalic, cmtbStroke,
    cmtbCodeBlock);
  {* 文本块需要配对的类型}

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

    function Add(AMarkDown: TCnMarkDownBase): TCnmarkDownBase;
    procedure Delete(Index: Integer);

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
  public
    property OpenType: TCnMarkDownTextBraceType read FOpenType write FOpenType;
    property CloseType: TCnMarkDownTextBraceType read FCloseType write FCloseType;

    property ParagraphType: TCnMarkDownParagraphType read FParagraphType write FParagraphType;
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
  protected
    function ConvertParagraphStart(Paragraph: TCnMarkDownParagraph): string; virtual; abstract;
    function ConvertParagraphEnd(Paragraph: TCnMarkDownParagraph): string; virtual; abstract;
    function ConvertFragment(Fragment: TCnMarkDownTextFragment): string; virtual; abstract;
    function EscapeContent(const Text: string): string; virtual; abstract;
  public
    function Convert(Root: TCnMarkDownBase): string; virtual; abstract;
  end;

  TCnRTFConverter = class(TCnMarkDownConverter)
  {* RTF 转换器实现类}
  private
    FRtf: TCnStringBuilder;
    FCurrentFontSize: Integer;
    FListCounters: array[1..9] of Integer; // 支持最多 9 级列表
    function GetListLevel(Paragraph: TCnMarkDownParagraph): Integer;
    function GetQuotaLevel(Paragraph: TCnMarkDownParagraph): Integer;
    {* 获得包括本节点在内的所有父节点层次中的引用嵌套数，0 开始}
  protected
    function ConvertParagraphStart(Paragraph: TCnMarkDownParagraph): string; override;
    function ConvertParagraphEnd(Paragraph: TCnMarkDownParagraph): string; override;
    function ConvertFragment(Fragment: TCnMarkDownTextFragment): string; override;
    function EscapeContent(const Text: string): string; override;
    procedure ProcessNode(Node: TCnMarkDownBase);
  public
    constructor Create;
    destructor Destroy; override;
    function Convert(Root: TCnMarkDownBase): string; override;
  end;

function CnParseMarkDownString(const MarkDown: string): TCnMarkDownBase;
{* 将 MarkDown 字符串解析为树状对象，返回对象需在外部不用时释放}

procedure CnMarkDownDebugOutput(MarkDown: TCnMarkDownBase; List: TStrings);
{* 将 MarkDown 对象树打印到字符串列表中}

function CnMarkDownConvertToRTF(Root: TCnMarkDownBase): string;
{* 将 MarkDown 的 DOM 树输出成 RTF 字符串}

//  RTF 段落格式：{\pard [控制参数] [文本内容] \par}

implementation

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
    '\red216\green216\blue216;}' +              // 颜色表（黑、红、灰）
    '\viewkind4\uc1' +                          // 视图模式+Unicode声明
    '\pard' +
    '\lang2052\langfe2052\f0\fs24\qj';          // 中文排版设置+默认字体

  CN_RTF_FOOTER = '}';

  CN_QUOTA_INDENT = 720;    // 引用的缩进
  CN_LIST_UNINDENT = -360;  // 列表的点或序号的反缩进
  CN_LIST_INDENT = 720;     // 列表文字的缩进

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
        FTokenID := cmtQuota;
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
      end;
    end;
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
  IndentStr: string;
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
    List.Add(IndentStr + '[Paragraph] ' + TypeName);
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
    I: Integer;
    F: TCnMarkDownTextFragment;
    B: TCnMarkDownTextBraceType;
  begin
    // 从后往前找 Parent 的 Fragment 里是否有开放的
    // cmtBold, cmtItalic, cmtBoldItalic, cmtStroke, cmtCodeBlock 等
    // 以决定本次遇到的是开还是闭，注意处理了交叉
    B := MapTokenToBrace(AnOpen);
    for I := Parent.Count - 1 downto 0 do
    begin
      F := TCnMarkDownTextFragment(Parent.Items[I]);
      if (F.FragmentType in CN_MARKDOWN_FRAGMENTTYPE_NEED_MATCH) and (F.OpenType = B) and (F.CloseType <> B) then
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

  PT := TCnMarkDownParagraph(Parent).ParagraphType;
  if PT in [cmpPre, cmpFenceCodeBlock] then
  begin
    // Pre 和代码块都原封不动处理单行，代码块由外界循环处理
    Frag := TCnMarkDownTextFragment.Create;
    Frag.FragmentType := cmfCommon;
    Parent.Add(Frag);

    repeat
      P.Next;
      Frag.AddContent(P.Token);
    until (P.TokenID in [cmtTerminate, cmtLineBreak, cmtHardBreak]); // 普通回车或硬回车结束
  end
  else
  begin
    if PT in [cmpHeading1..cmpHeading7, cmpOrderedList, cmpUnOrderedList, cmpLine] then // 这几个段落有开始标记，跳过
      P.Next;

    // 循环解析行内容
    while not (P.TokenID in [cmtTerminate, cmtHardBreak]) do
    begin
      case P.TokenID of
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
      P.Next;
    end;
  end;
end;

function CnParseMarkDownString(const MarkDown: string): TCnMarkDownBase;
var
  I: Integer;
  P: TCnMarkDownParser;
  Root: TCnMarkDownBase;
  CurPara: TCnMarkDownParagraph;
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
        else //cmtContent, cmtLinkDisplay, cmtDirectLink, cmtImageSign:
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

  // 先清除掉无用的空段
  if Root <> nil then
  begin
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

{ TCnRTFConverter }

function TCnRTFConverter.Convert(Root: TCnMarkDownBase): string;
var
  I: Integer;
begin
  FRtf.Clear;
  FRtf.Append(CN_RTF_HEADER);

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
  Level, Q: Integer;
begin
  Q := GetQuotaLevel(Paragraph);
  case Paragraph.ParagraphType of
    cmpHeading1..cmpHeading7:
      begin
        FCurrentFontSize := 36 - (Ord(Paragraph.ParagraphType) - Ord(cmpHeading1)) * 4;
        Result := Format('{\pard\fs%d\b ', [FCurrentFontSize]);
      end;
    cmpUnorderedList:
      begin
        Result := Format('{\pard{\pntext\f2\''B7\tab}{\*\pn\pnlvlblt\pnf2\pnindent0{\pntxtb\''B7}}\fi%d\li%d\sa200\sl276\slmult1 ',
          [CN_LIST_UNINDENT, CN_LIST_INDENT + Q * CN_QUOTA_INDENT]);
      end;
    cmpOrderedList:
      begin
        Level := GetListLevel(Paragraph);  // 列表缩进层级
        Q := GetQuotaLevel(Paragraph);     // 引用缩进层级
        Inc(FListCounters[Level]);         // 列表序号

        Result := Format('{\pard{\pntext\f0 %d.\tab}\fi%d\li%d\sa200\sl276\slmult1\lang2052\f0\fs22 ',
          [FListCounters[Level], CN_LIST_UNINDENT, CN_LIST_INDENT + Q * CN_QUOTA_INDENT])
      end;
    cmpLine:
      Result := '{\pard\sa200\brdrb\brdrs\brdrw15\par}';
    cmpQuota:
      begin
        // Quota 本身不是具体段落，不写
      end;
    cmpFenceCodeBlock:
      Result := '{\lang2052\f1\fs20\cbpat1 ';
  else
    if (Paragraph.Parent <> nil) and (Paragraph.Parent is TCnMarkDownParagraph) then
    begin
      Result := Format('{\pard\li%d ', [Q * CN_QUOTA_INDENT])   // 普通内层段落缩进由引用层级控制
    end
    else
      Result := '{\pard\plain\li0\fi0 '; // 顶级段落用默认缩进 0 的格式
  end;
end;

constructor TCnRTFConverter.Create;
begin
  inherited;
  FRtf := TCnStringBuilder.Create;
  FCurrentFontSize := 24;
end;

destructor TCnRTFConverter.Destroy;
begin
  FRtf.Free;
  inherited;
end;

function TCnRTFConverter.EscapeContent(const Text: string): string;
begin
  Result := StringReplace(Text, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '{', '\{', [rfReplaceAll]);
  Result := StringReplace(Result, '}', '\}', [rfReplaceAll]);
  Result := StringReplace(Result, #13#10, '\line ', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\line ', [rfReplaceAll]);
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

function CnMarkDownConvertToRTF(Root: TCnMarkDownBase): string;
begin
  with TCnRTFConverter.Create do
  try
    Result := Convert(Root);
  finally
    Free;
  end;
end;

end.

