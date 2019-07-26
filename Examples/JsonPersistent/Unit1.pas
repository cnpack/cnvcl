unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CnJsonPersistent, Generics.Defaults, Generics.Collections, StdCtrls;

type
  TForm1 = class(TForm)
    mmo1: TMemo;
    mmo2: TMemo;
    btnTest: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TTest = class(TCnJsonPersistent)
  public
    [CnJsonPersistentAttr(False)]
    dummy: string;
    sString: string;
    sAnsiChar: AnsiChar;
    sWideChar: WideChar;
    sAnsiString: AnsiString;
    sWideString: WideString;
    sByte: Byte;
    sShortInt: ShortInt;
    sWord: Word;
    sSmallInt: SmallInt;
    sInteger: Integer;
    sCardinal: Cardinal;
    sInt64: Int64;
    sUInt64: UInt64;
    sSingle: Single;
    sDouble: Double;
    sExtended: Extended;
    sBoolean: Boolean;
    sByteBool: ByteBool;
    sWordBool: WordBool;
    sLongBool: LongBool;
    sEnum: TFormStyle;
    sDateTime: TDateTime;
    sDate: TDate;
    sTime: TTime;
  end;

  TTest2 = class(TCnJsonPersistent)
  public
    IntList: TIntegerList;
    StringList: TStringList;
    sObject: TTest;
  end;

  TTest2List = class(TCnJsonPersistentObjectList<TTest2>);

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnTestClick(Sender: TObject);
var
  Test2: TTest2;
  Test2List, Test2List2: TTest2List;
  s: string;
  i, j: Integer;

  function RandomStr: string;
  var
    i: Integer;
  begin
    Result := '';
    for i := 0 to Random(100) + 2 do
      Result := Result + Char(32 + Random(127 - 32));
  end;

  function RandomInt(Max: Integer = MaxInt): Int64;
  begin
    if Max = MaxInt then
      Result := Random(Max) * Random(Max)
    else
      Result := Random(Max);
    if Odd(Random(MaxInt)) then
      Result := Result + 1
    else
      Result := - Result - 1;
  end;
begin
  Test2List := TTest2List.Create;
  Test2List2 := TTest2List.Create;
  try
    for i := 0 to Random(5) do
    begin
      Test2 := TTest2.Create;
      with Test2.sObject do
      begin
        dummy := RandomStr;
        sString := RandomStr;
        sAnsiChar := AnsiChar(RandomStr[1]);
        sWideChar := RandomStr[1];
        sAnsiString := RandomStr;
        sWideString := RandomStr;
        sByte := RandomInt;
        sShortInt := RandomInt;
        sWord := RandomInt;
        sSmallInt := RandomInt;
        sInteger := RandomInt;
        sCardinal := RandomInt;
        sInt64 := RandomInt;
        sUInt64 := RandomInt;
        sSingle := RandomInt / RandomInt;
        sDouble := RandomInt / RandomInt;
        sExtended := RandomInt / RandomInt;
        sBoolean := True;// Odd(RandomInt);
        sByteBool := True;// Odd(RandomInt);
        sWordBool := True;// Odd(RandomInt);
        sLongBool := True;// Odd(RandomInt);
        sEnum := TFormStyle(RandomInt(Ord(High(TFormStyle)) + 1));
        sDateTime := Now + RandomInt(5000) / 100;
        sDate := Date + RandomInt(1000);
        sTime := Time + RandomInt(5000) / 5000;
      end;
      for j := 0 to Random(10) do
        Test2.IntList.Add(RandomInt);
      for j := 0 to Random(10) do
        Test2.StringList.Add(RandomStr);
      Test2List.Add(Test2);
    end;
    Test2List.SaveToJsonString(s, False);
    mmo1.Lines.Text := s;

    Test2List2.LoadFromJsonString(s);
    Test2List2.SaveToJsonString(s, False);
    mmo2.Lines.Text := s;
  finally
    Test2List.Free;
    Test2List2.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  CnJsonPersistentEnableDbgLog := True;
end;

end.
