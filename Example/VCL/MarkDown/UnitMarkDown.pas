unit UnitMarkDown;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, TypInfo, CnMarkDown, CnMarkDownView;

type
  TFormMarkDown = class(TForm)
    pgcMarkDown: TPageControl;
    tsParser: TTabSheet;
    tsVirtual: TTabSheet;
    pnlVirtualTools: TPanel;
    btnStartVirtualStream: TButton;
    btnLoadVirtualHistory: TButton;
    btnLoadVirtualSamples: TButton;
    btnLoadVirtualPerformance: TButton;
    btnAppendVirtualUpdate: TButton;
    btnExpandVirtualItem: TButton;
    tmrVirtualStream: TTimer;
    mmoMarkDown: TMemo;
    redtMarkDown: TRichEdit;
    btnTest: TButton;
    mmoParse: TMemo;
    btnDump: TButton;
    btnParseTree: TButton;
    btnConvRtf: TButton;
    btnShowRTF: TButton;
    lblFontSize: TLabel;
    edtBaseFontSize: TEdit;
    procedure btnTestClick(Sender: TObject);
    procedure btnDumpClick(Sender: TObject);
    procedure btnParseTreeClick(Sender: TObject);
    procedure btnConvRtfClick(Sender: TObject);
    procedure btnShowRTFClick(Sender: TObject);
    procedure btnStartVirtualStreamClick(Sender: TObject);
    procedure btnLoadVirtualHistoryClick(Sender: TObject);
    procedure btnLoadVirtualSamplesClick(Sender: TObject);
    procedure btnLoadVirtualPerformanceClick(Sender: TObject);
    procedure btnAppendVirtualUpdateClick(Sender: TObject);
    procedure btnExpandVirtualItemClick(Sender: TObject);
    procedure tmrVirtualStreamTimer(Sender: TObject);
  private
    FVirtualView: TCnMarkDownView;
    FVirtualFeed: TCnMarkDownFeed;
    FStreamIndex: Integer;
    FStreamRemaining: Integer;
    FStreamChunk: TCnMarkDownText;
    procedure DumpMarkDownTokens(const MD: string);
    function MakeRandomChineseText(CharCount: Integer): TCnMarkDownText;
    function BuildVirtualPerformanceSample(AIndex: Integer): TCnMarkDownText;
    function BuildRandomUpdateSample(AIndex: Integer): TCnMarkDownText;
    function BuildRandomItemExpansionSample(AIndex: Integer): TCnMarkDownText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormMarkDown: TFormMarkDown;

implementation

{$R *.DFM}

constructor TFormMarkDown.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FVirtualFeed := TCnMarkDownFeed.Create;

  FVirtualView := TCnMarkDownView.Create(Self);
  FVirtualView.Parent := tsVirtual;
  FVirtualView.Align := alClient;
  FVirtualView.Feed := FVirtualFeed;
  pnlVirtualTools.BringToFront;
end;

destructor TFormMarkDown.Destroy;
begin
  FVirtualView.Free;
  FVirtualFeed.Free;
  inherited Destroy;
end;

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
  S: AnsiString;
  Mem: TMemoryStream;
begin
  mmoParse.Lines.Clear;
  MD := CnParseMarkDownString(mmoMarkDown.Lines.Text);
  S := CnMarkDownConvertToRTF(MD, StrToIntDef(edtBaseFontSize.Text, 12));
  mmoParse.Lines.Text := string(S);
  MD.Free;

  if Length(S) > 0 then
  begin
    Mem := TMemoryStream.Create;
    Mem.WriteBuffer(S[1], Length(S));
    Mem.Position := 0;
    redtMarkDown.Lines.LoadFromStream(Mem);
    Mem.Free;
  end;
end;

procedure TFormMarkDown.btnShowRTFClick(Sender: TObject);
var
  S: AnsiString;
  Mem: TMemoryStream;
begin
  S := mmoParse.Lines.Text;
  if Length(S) > 0 then
  begin
    Mem := TMemoryStream.Create;
    Mem.WriteBuffer(S[1], Length(S));
    Mem.Position := 0;
    redtMarkDown.Lines.LoadFromStream(Mem);
    Mem.Free;
  end;
end;

procedure TFormMarkDown.btnStartVirtualStreamClick(Sender: TObject);
begin
  tmrVirtualStream.Enabled := False;
  FVirtualFeed.Clear;
  FStreamIndex := FVirtualView.AddMessage(cmrAssistant);
  FStreamRemaining := 1024 * 1024;
  FStreamChunk := TCnMarkDownText(StringOfChar('x', 4092) + #13#10);
  tmrVirtualStream.Enabled := True;
end;

procedure TFormMarkDown.tmrVirtualStreamTimer(Sender: TObject);
var
  Count: Integer;
begin
  Count := Length(FStreamChunk);
  if Count > FStreamRemaining then
    Count := FStreamRemaining;
  if Count > 0 then
  begin
    FVirtualView.AppendToMessage(FStreamIndex,
      Copy(FStreamChunk, 1, Count));
    Dec(FStreamRemaining, Count);
  end;
  if FStreamRemaining <= 0 then
  begin
    FVirtualView.FinishMessage(FStreamIndex);
    tmrVirtualStream.Enabled := False;
  end;
end;

procedure TFormMarkDown.btnLoadVirtualHistoryClick(Sender: TObject);
var
  I: Integer;
  Payload: TCnMarkDownText;
begin
  tmrVirtualStream.Enabled := False;
  FVirtualFeed.Clear;
  Payload := TCnMarkDownText(StringOfChar('x', 5116) + #13#10#13#10);
  FVirtualFeed.BeginUpdate;
  FVirtualView.BeginUpdate;
  try
    for I := 0 to 9999 do
    begin
      FVirtualFeed.AddMessage(cmrAssistant);
      FVirtualFeed.AppendMessage(I, Payload);
      FVirtualFeed.FinishMessage(I);
    end;
  finally
    FVirtualFeed.EndUpdate;
    FVirtualView.EndUpdate;
  end;
  FVirtualView.Flush;
end;

procedure TFormMarkDown.btnLoadVirtualSamplesClick(Sender: TObject);
const
  SampleCount = 12;
var
  Samples: array[0..SampleCount - 1] of TCnMarkDownText;
  I: Integer;
begin
  Samples[0] := '# 标题与段落'#13#10 +
    '这是一个带有中文内容的普通段落，用于测试标题后的排版。';
  Samples[1] := '**粗体**、*斜体*、~~删除线~~和`行内代码`。';
  Samples[2] := '链接：[CnPack](https://www.cnpack.org)'#13#10 +
    '图片占位：![Logo](logo.png)';
  Samples[3] := '- 无序列表第一项'#13#10 +
    '- 无序列表第二项'#13#10 +
    '+ 使用加号的列表项';
  Samples[4] := '1. 有序列表第一项'#13#10 +
    '2. 有序列表第二项'#13#10 +
    '10. 用两位数字测试编号宽度';
  Samples[5] := '> 一级引用内容'#13#10 +
    '> 引用的第二行'#13#10 +
    '> > 嵌套引用内容';
  Samples[6] := '---'#13#10 +
    '分隔线下方的段落，用来检查上下间距。';
  Samples[7] := '```pascal'#13#10 +
    'function Add(A, B: Integer): Integer;'#13#10 +
    'begin'#13#10 +
    '  Result := A + B;'#13#10 +
    'end;'#13#10 +
    '```';
  Samples[8] := '这一行末尾有两个空格，后面会强制换行。  '#13#10 +
    '这是下一行；普通换行也用于测试。';
  Samples[9] := '中文、English、12345 以及 UTF-16 文本。'#13#10 +
    '第二行用于测试中英文混排和自动换行。';
  Samples[10] := '***粗斜体***、___组合格式___，以及[带格式的链接](https://example.com)。';
  Samples[11] := '长文本测试：这条消息故意包含较长的连续文本，用于观察 RichEdit 宿主的换行、'
    + '高度测量和虚拟滚动效果。滚动到其他消息后再返回，检查视口位置是否保持稳定。';

  tmrVirtualStream.Enabled := False;
  FVirtualFeed.Clear;
  FVirtualFeed.BeginUpdate;
  FVirtualView.BeginUpdate;
  try
    for I := 0 to SampleCount - 1 do
    begin
      FVirtualFeed.AddMessage(cmrAssistant);
      FVirtualFeed.AppendMessage(I, Samples[I]);
      FVirtualFeed.FinishMessage(I);
    end;
  finally
    FVirtualFeed.EndUpdate;
    FVirtualView.EndUpdate;
  end;
  FVirtualView.Flush;
end;

function TFormMarkDown.MakeRandomChineseText(
  CharCount: Integer): TCnMarkDownText;
var
  CharPool: TCnMarkDownText;
  I: Integer;
begin
  CharPool := '天地山水日月星云春夏秋冬风雨花草树木江河湖海城镇乡村道路桥梁'
    + '文章文字语言思想知识技术程序数据界面窗口消息内容测试显示滚动排版'
    + '速度容量稳定清晰自然丰富简单复杂开发设计运行结果用户系统功能';
  if CharCount < 0 then
    CharCount := 0;
  SetLength(Result, CharCount);
  for I := 1 to CharCount do
    Result[I] := CharPool[Random(Length(CharPool)) + 1];
end;

function TFormMarkDown.BuildVirtualPerformanceSample(
  AIndex: Integer): TCnMarkDownText;
var
  N, Body: TCnMarkDownText;
  I, ParagraphCount: Integer;
begin
  N := TCnMarkDownText(IntToStr(AIndex + 1));

  { 每八条生成一条超过一千个汉字的多段长文。 }
  if AIndex mod 8 = 0 then
  begin
    Result := '# 超长分段条目 ' + N + #13#10#13#10;
    ParagraphCount := 7 + AIndex mod 3;
    for I := 1 to ParagraphCount do
    begin
      Body := MakeRandomChineseText(180 + Random(81));
      case I mod 4 of
        0: Result := Result + '## 分段标题 ' +
          TCnMarkDownText(IntToStr(I)) + #13#10 + Body;
        1: Result := Result + '**粗体开头** ' + Body;
        2: Result := Result + '> 长引用段落 ' + Body;
        3: Result := Result + '- 长列表段落 ' + Body;
      end;
      Result := Result + #13#10#13#10;
    end;
    Exit;
  end;

  Body := MakeRandomChineseText(30 + Random(71));
  { 跳过长文条目后仍能循环覆盖全部十六种短消息样式。 }
  case (AIndex + AIndex div 8) mod 16 of
    0: Result := '# 标题 ' + N + #13#10#13#10 + Body;
    1: Result := '**粗体 ' + N + '**、*斜体*、~~删除线~~与`code`。'#13#10 + Body;
    2: Result := '[CnPack ' + N + '](https://www.cnpack.org) '
      + '![图片占位](logo.png)'#13#10 + Body;
    3: Result := '- 无序项目 ' + N + #13#10 + '- ' + Body + #13#10
      + '+ 加号列表项';
    4: Result := '1. 有序项目 ' + N + #13#10 + '2. ' + Body + #13#10
      + '10. 两位数编号';
    5: Result := '> 引用条目 ' + N + #13#10 + '> ' + Body + #13#10
      + '> > 嵌套引用';
    6: Result := '分隔线上方 ' + N + #13#10#13#10 + '---'#13#10#13#10 + Body;
    7: Result := '```pascal'#13#10 + 'procedure Test' + N + ';'#13#10
      + 'begin'#13#10 + '  WriteLn(''' + N + ''');'#13#10 + 'end;'#13#10
      + '```'#13#10 + Body;
    8: Result := '行内代码 `Item[' + N + ']` 与转义字符 \* \_ \#。'#13#10 + Body;
    9: Result := '| 编号 | 内容 |'#13#10 + '| ---: | :--- |'#13#10 + '| ' + N
      + ' | ' + Body + ' |';
    10: Result := '强制换行 ' + N + '。  '#13#10 + Body + '  '#13#10
      + '换行结束。';
    11: Result := '- 父列表 ' + N + #13#10 + '  - 子列表一'#13#10
      + '  - ' + Body;
    12: Result := '# 一级标题 ' + N + #13#10 + '## 二级标题'#13#10
      + '### 三级标题'#13#10 + Body;
    13: Result := '中英混排 ' + N + ': Markdown, Delphi, 1234567890。'#13#10 + Body;
    14: Result := '<https://www.cnpack.org> 直接链接 ' + N + #13#10 + Body;
    15: Result := '    function Item' + N + ': Integer;'#13#10
      + '    begin'#13#10 + '      Result := ' + N + ';'#13#10 + '    end;'#13#10
      + Body;
  end;
end;

function TFormMarkDown.BuildRandomUpdateSample(AIndex: Integer): TCnMarkDownText;
var
  N, Body: TCnMarkDownText;
begin
  N := TCnMarkDownText(IntToStr(AIndex));
  Body := MakeRandomChineseText(40 + Random(121));
  case Random(8) of
    0: Result := '### 局部追加标题 ' + N + #13#10#13#10 + Body;
    1: Result := '**局部更新** ' + Body + '。';
    2: Result := '- 追加列表项 ' + N + #13#10 + '- ' + Body;
    3: Result := '> 追加引用 ' + Body;
    4: Result := '```pascal'#13#10 +
      'procedure LocalUpdate' + N + ';'#13#10 +
      'begin'#13#10 +
      '  Result := ' + N + ';'#13#10 +
      'end;'#13#10 + '```';
    5: Result := '[追加链接](https://www.cnpack.org) ' + Body;
    6: Result := '| 字段 | 内容 |'#13#10 + '| --- | --- |'#13#10 +
      '| 更新 | ' + Body + ' |';
  else
    Result := Body + '。  '#13#10 + '局部追加的第二行，用于测试重排版。';
  end;
end;

function TFormMarkDown.BuildRandomItemExpansionSample(
  AIndex: Integer): TCnMarkDownText;
var
  N, Body, Extra: TCnMarkDownText;
begin
  N := TCnMarkDownText(IntToStr(AIndex + 1));
  Body := MakeRandomChineseText(120 + Random(181));
  Extra := MakeRandomChineseText(80 + Random(121));
  { 这里追加到已有块内部，不再引入新的 Markdown 块，便于观察单条目扩高。 }
  Result := ' ' + Body + ' **扩展 Item ' + N +
    '**、`inline-' + N + '`。' + #10 + Extra + '。';
end;

procedure TFormMarkDown.btnLoadVirtualPerformanceClick(Sender: TObject);
const
  SampleCount = 200;
var
  I: Integer;
  Sample: TCnMarkDownText;
begin
  tmrVirtualStream.Enabled := False;
  FVirtualFeed.Clear;
  Randomize;
  FVirtualFeed.BeginUpdate;
  FVirtualView.BeginUpdate;
  try
    for I := 0 to SampleCount - 1 do
    begin
      Sample := BuildVirtualPerformanceSample(I);
      FVirtualFeed.AddMessage(cmrAssistant);
      FVirtualFeed.AppendMessage(I, Sample);
      FVirtualFeed.FinishMessage(I);
    end;
  finally
    FVirtualFeed.EndUpdate;
    FVirtualView.EndUpdate;
  end;
  FVirtualView.Flush;
end;

procedure TFormMarkDown.btnAppendVirtualUpdateClick(Sender: TObject);
var
  MessageIndex: Integer;
  Extra: TCnMarkDownText;
begin
  MessageIndex := FVirtualView.SelectedMessageIndex;
  if MessageIndex < 0 then
  begin
    MessageDlg('Please Click or Select a Visible Markdown Item First.',
      mtInformation, [mbOK], 0);
    Exit;
  end;
  Randomize;
  Extra := #13#10#13#10 +
    BuildRandomUpdateSample(MessageIndex + 1);
  tmrVirtualStream.Enabled := False;
  FVirtualView.AppendToMessage(MessageIndex, Extra);
  FVirtualFeed.FlushMessageQueue(MessageIndex, 0);
  FVirtualFeed.FinishMessage(MessageIndex);
  { 测试按钮需要立即看到局部条目重排结果，不等待流式定时器。 }
  FVirtualView.Flush;
end;

procedure TFormMarkDown.btnExpandVirtualItemClick(Sender: TObject);
var
  ItemIndex: Integer;
  Extra: TCnMarkDownText;
begin
  ItemIndex := FVirtualView.SelectedItemIndex;
  if ItemIndex < 0 then
  begin
    MessageDlg('Please click or select a visible Markdown item first.',
      mtInformation, [mbOK], 0);
    Exit;
  end;
  Randomize;
  Extra := BuildRandomItemExpansionSample(ItemIndex);
  tmrVirtualStream.Enabled := False;
  FVirtualView.AppendToItem(ItemIndex, Extra);
  { 直接修改已有块后立即重新测量，确保只观察当前条目的局部重排。 }
  FVirtualView.Flush;
end;

end.
