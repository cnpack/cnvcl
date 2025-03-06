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
* 备    注：语法支持不完整，譬如没有表格
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
  Classes, SysUtils;

type
  TCnMDTokenType = (cmtUnknown,
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
    cmtLineBreak,      // 回车换行
    cmtTerminate       // 结束符
  );

  TCnMarkDownParser = class
  {* String 格式的 MarkDown 字符串语法解析器}
  private
    FRun: Integer;
    FTokenPos: Integer;
    FOrigin: PChar;
    FIsLineStart: Boolean;
    FTokenID: TCnMDTokenType;

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
    function IsCRLF(C: Char): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    function IsSpaceOrTab(C: Char): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
  protected
    function TokenEqualStr(Org: PChar; const Str: string): Boolean;
    procedure StepRun; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure Next;
    {* 跳至下一个 Token 并确定 TokenID}
    procedure NextNoJunk;
    {* 跳至下一个非 Null 以及非空格 Token 并确定 TokenID}

    property Origin: PAnsiChar read FOrigin write SetOrigin;
    {* 待解析的 string 格式的 MarkDown 字符串内容}
    property RunPos: Integer read FRun;
    {* 当前处理位置相对于 FOrigin 的线性偏移量，单位为字节数，0 开始}
    property TokenID: TCnMDTokenType read FTokenID;
    {* 当前 Token 类型}
    property Token: string read GetToken;
    {* 当前 Token 的原始字符串，暂不解析转义内容}
    property TokenLength: Integer read GetTokenLength;
    {* 当前 Token 的字节长度}
  end;

implementation

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
  end;
end;

function TCnMarkDownParser.IsCRLF(C: Char): Boolean;
begin
  Result := (C = #13) or (C = #10);
end;

function TCnMarkDownParser.IsSpaceOrTab(C: Char): Boolean;
begin
  Result := (C = ' ') or (C = #9);
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

procedure TCnMarkDownParser.NextNoJunk;
begin
  Next;
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
  FTokenID := cmtContent;

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

function TCnMarkDownParser.TokenEqualStr(Org: PChar; const Str: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Length(Str) - 1 do
  begin
    if Org[I] <> Str[I + 1] then
    begin
      Result := False;
      Exit;
    end;
  end;
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

end.

