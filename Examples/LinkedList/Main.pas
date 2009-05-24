unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CnLinkedList;

type
  TFrmMain = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure AddStr(const Value: string);
  end;

type
  TTestThread = class(TThread)
  protected // 线程基类
    AStr: string;
    procedure AddStr;
  public
    constructor Create;
  end;

type
  TTestThread1 = class(TTestThread)
  protected // 正向遍历
    procedure Execute(); override;
  end;

type
  TTestThread2 = class(TTestThread)
  protected // 反向遍历
    procedure Execute(); override;
  end;

var
  FrmMain: TFrmMain;
  AList: TCnLinkedList;
  IsMultiThreadStart: Boolean = False;

implementation

{$R *.dfm}

procedure TFrmMain.FormCreate(Sender: TObject);
var
  ChrLoop1, ChrLoop2: Char;
  Temp: string;
  CurrIndex: DWORD;
begin
  AList:= TCnLinkedList.Create;

  CurrIndex:= 0;
  for ChrLoop1 := '0' to '1' do
    for ChrLoop2 := '0' to '9' do
      begin
        Temp:= '第' + FormatFloat('000', CurrIndex) + '项"' + ChrLoop1 + ChrLoop2 + '"';
        AList.Add(StrNew(PAnsiChar(Temp)));
        Inc(CurrIndex);
      end;
end;

procedure TFrmMain.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Clear;

  TTestThread1.Create;
  TTestThread2.Create;
  TTestThread1.Create;
  TTestThread2.Create;
  TTestThread1.Create;
  TTestThread2.Create;
  TTestThread2.Create;
end;

procedure TFrmMain.Button2Click(Sender: TObject);
var
  Ret:Integer;
begin
  Ret:=AList.Insert(10,StrNew(PAnsiChar('Added')));
  if Ret > -1 then
    Button4.Click
  else// 遍历输出
    Self.AddStr('Insert error!');
end;

procedure TFrmMain.Button3Click(Sender: TObject);
var
  Ret: Integer;
begin
  Ret := AList.Delete(10);
  if Ret > -1 then
    Button4.Click
  else// 遍历输出
    Self.AddStr('Delete error!');
end;

procedure TFrmMain.Button4Click(Sender: TObject);
var
i:integer;
begin
{$DEFINE FromHead}
{$IFDEF FromHead}
//从头遍历
  for I := 0 to AList.Count - 1 do
    Self.AddStr(PAnsiChar(AList.Items[i]))
{$ELSE}
  //从尾遍历
  for I := AList.Count - 1 downto 0 do
    Self.AddStr(PAnsiChar(AList.Items[i]))
{$ENDIF}
end;


procedure TFrmMain.Button5Click(Sender: TObject);
begin
  Self.AddStr('交换 5，15，两个元素,之前');
  Button4.Click;
  AList.Exchange(5,15);
  Self.AddStr('交换 5，15，两个元素,之后');
  Button4.Click;
end;

procedure TFrmMain.AddStr(const Value: string);
begin
  Memo1.Lines.Add(Value);
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
var
  Loop: DWORD;
begin
  for Loop := AList.Count - 1 downto 0 do
    StrDispose(AList.Items[Loop]);
  FreeAndNil(AList);
end;

{ TTestThread }

procedure TTestThread.AddStr;
begin
  if AStr <> '' then
    FrmMain.AddStr(AStr);
end;

constructor TTestThread.Create;
begin
  inherited Create(True);
  AStr:= '';
  FreeOnTerminate := True;
  Resume;
end;

{ TTestThread1 }

procedure TTestThread1.Execute;
var
  CurrIndex: DWORD;
  Iterator: ICnLinkedListIterator;
begin
  CurrIndex:= 0;
  Iterator:= AList.CreateIterator;
  Iterator.First;
  while not Iterator.Eof do
    begin
      AStr:= '线程 ' + Format('%8.8x', [GetCurrentThreadId]) + ' 读取的第' +
        FormatFloat('000', CurrIndex) + '项为: ' + PAnsiChar(Iterator.GetCurrentItem);
      Synchronize(AddStr);
      AStr:= '';
      Iterator.Next;
      Inc(CurrIndex);
    end;
end;

{ TTestThread2 }

procedure TTestThread2.Execute;
var
  CurrIndex: DWORD;
  Iterator: ICnLinkedListIterator;
begin
  CurrIndex:= AList.Count - 1;
  Iterator:= AList.CreateIterator;
  Iterator.Last;
  while not Iterator.Bof do
    begin
      AStr:= '线程 ' + Format('%8.8x', [GetCurrentThreadId]) + ' 读取的第' +
        FormatFloat('000', CurrIndex) + '项为: ' + PAnsiChar(Iterator.GetCurrentItem);
      Synchronize(AddStr);
      AStr:= '';
      Iterator.Previous;
      Dec(CurrIndex);
    end;
end;

end.
