unit UnitMarkDown;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, TypInfo;

type
  TFormMarkDown = class(TForm)
    mmoMarkDown: TMemo;
    redtMarkDown: TRichEdit;
    btnTest: TButton;
    mmoParse: TMemo;
    btnDump: TButton;
    btnParseTree: TButton;
    btnConvRtf: TButton;
    procedure btnTestClick(Sender: TObject);
    procedure btnDumpClick(Sender: TObject);
    procedure btnParseTreeClick(Sender: TObject);
    procedure btnConvRtfClick(Sender: TObject);
  private
    procedure DumpMarkDownTokens(const MD: string);
  public
    { Public declarations }
  end;

var
  FormMarkDown: TFormMarkDown;

implementation

{$R *.DFM}

uses
  CnMarkDown;

procedure TFormMarkDown.btnTestClick(Sender: TObject);
//const
//  SampleMD =
//    '# 标题'#13#10 +
//    '这是第一段落的第一行  '#13#10 +  // 两个空格强制换行
//    '这是同一段落的第二行\'#13#10 +  // 反斜杠换行
//    '这是普通换行'#13#10 +
//    ''#13#10 +  // 空行分隔段落
//    '* 列表项1'#13#10 +
//    '* 列表项2'#13#10 +
//    ''#13#10 +
//    '> 引用内容'#13#10 +
//    ''#13#10 +
//    '```delphi'#13#10 +
//    'procedure Test;'#13#10 +
//    'begin'#13#10 +
//    '  ShowMessage(''Hello'');'#13#10 +
//    'end;'#13#10 +
//    '```'#13#10 +
//    ''#13#10 +
//    '链接示例：[CnPack](https://www.cnpack.org) 图片示例：![Logo](logo.png)';

const
  SampleMD =
    '# Header'#13#10  +
    '1st Para lst line  '#13#10 +  // 两个空格强制换行
    '1st para 2nd line\'#13#10 +  // 反斜杠换行
    'com*mon* line'#13#10 +
    ''#13#10 +  // 空行分隔段落
    '* Ulist1'#13#10 +
    '* ulist2'#13#10 +
    ''#13#10 +
    '> quota'#13#10 +
    ''#13#10 +
    '```delphi'#13#10 +
    'procedure Test;'#13#10 +
    'begin'#13#10 +
    '  ShowMessage(''Hello'');'#13#10 +
    'end;'#13#10 +
    '```'#13#10 +
    ''#13#10 +
    'Link：[CnPack](https://www.cnpack.org) Picture: ![Logo](logo.png)';
begin

end;

procedure TFormMarkDown.DumpMarkDownTokens(const MD: string);
var
  I: Integer;
  Parser: TCnMarkDownParser;
begin
  Parser := TCnMarkDownParser.Create;
  try
    Parser.Origin := PChar(MD);

    I := 1;
    while Parser.TokenID <> cmtTerminate do
    begin
      mmoParse.Lines.Add(Format('%3.3d. Length %3.3d, Pos %4.4d. %s, Token: %s',
        [I, Parser.TokenLength, Parser.RunPos, GetEnumName(TypeInfo(TCnMarkDownTokenType),
         Ord(Parser.TokenID)), Parser.Token]));
      Parser.Next;
      Inc(I);
    end;
  finally
    Parser.Free;
  end;
end;

procedure TFormMarkDown.btnDumpClick(Sender: TObject);
begin
  mmoParse.Lines.Clear;
  DumpMarkDownTokens(mmoMarkDown.Lines.Text);
end;

procedure TFormMarkDown.btnParseTreeClick(Sender: TObject);
var
  MD: TCnMarkDownBase;
begin
  mmoParse.Lines.Clear;
  MD := CnParseMarkDownString(mmoMarkDown.Lines.Text);
  CnMarkDownDebugOutput(MD, mmoParse.Lines);
  MD.Free;
end;

procedure TFormMarkDown.btnConvRtfClick(Sender: TObject);
var
  MD: TCnMarkDownBase;
begin
  mmoParse.Lines.Clear;
  MD := CnParseMarkDownString(mmoMarkDown.Lines.Text);
  mmoParse.Lines.Text := CnMarkDownConvertToRTF(MD);
  MD.Free;
end;

end.
