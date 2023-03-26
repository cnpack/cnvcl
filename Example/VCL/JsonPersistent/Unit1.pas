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
    // Declare MUST be TIntegerList
    IntList: TIntegerList;
    // Declare MUST be TStringList
    StringList: TStringList;
    sObject: TTest;
  end;

  TTest2List = class(TCnJsonPersistentObjectList<TTest2>);

  TTest3 = class(TCnJsonPersistent)
  public
    sString: string;
    // Declare MUST be TCnJsonPersistentObjectList<xxx>
    sList: TCnJsonPersistentObjectList<TTest2>;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnTestClick(Sender: TObject);
var
  Test2: TTest2;
  Test2List, Test2List2: TTest2List;
  Test3, Test32: TTest3;
  s: string;
  i: Integer;

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
    begin
      Result := Random(Max) * Random(Max);
      if Odd(Random(MaxInt)) then
        Result := Result + 1
      else
        Result := - Result - 1;
    end
    else
      Result := Random(Max);
  end;

  function FloatEqu(V1, V2: Extended): Boolean;
  begin
    Result := Abs(V1 - V2) < 0.00001;
  end;

  procedure FillTest2(Obj: TTest2);
  var
    i: Integer;
  begin
    with Obj.sObject do
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
      sBoolean := Odd(RandomInt);
      sByteBool := Odd(RandomInt);
      sWordBool := Odd(RandomInt);
      sLongBool := Odd(RandomInt);
      sEnum := TFormStyle(RandomInt(Ord(High(TFormStyle))));
      sDateTime := Now + RandomInt(5000) / 100;
      sDate := Date + RandomInt(1000);
      sTime := Time + RandomInt(5000) / 5000;
    end;
    for i := 0 to Random(10) do
      Obj.IntList.Add(RandomInt);
    for i := 0 to Random(10) do
      Obj.StringList.Add(RandomStr);
  end;

  procedure CheckTest2(Obj1, Obj2: TTest2);
  var
    i: Integer;
  begin
    Assert(Obj1.sObject.dummy <> Obj2.sObject.dummy);
    Assert(Obj1.sObject.sString = Obj2.sObject.sString);
    Assert(Obj1.sObject.sAnsiChar = Obj2.sObject.sAnsiChar);
    Assert(Obj1.sObject.sWideChar = Obj2.sObject.sWideChar);
    Assert(Obj1.sObject.sAnsiString = Obj2.sObject.sAnsiString);
    Assert(Obj1.sObject.sWideString = Obj2.sObject.sWideString);
    Assert(Obj1.sObject.sByte = Obj2.sObject.sByte);
    Assert(Obj1.sObject.sShortInt = Obj2.sObject.sShortInt);
    Assert(Obj1.sObject.sWord = Obj2.sObject.sWord);
    Assert(Obj1.sObject.sSmallInt = Obj2.sObject.sSmallInt);
    Assert(Obj1.sObject.sInteger = Obj2.sObject.sInteger);
    Assert(Obj1.sObject.sCardinal = Obj2.sObject.sCardinal);
    Assert(Obj1.sObject.sInt64 = Obj2.sObject.sInt64);
    Assert(Obj1.sObject.sUInt64 = Obj2.sObject.sUInt64);
    Assert(FloatEqu(Obj1.sObject.sSingle, Obj2.sObject.sSingle));
    Assert(FloatEqu(Obj1.sObject.sDouble, Obj2.sObject.sDouble));
    Assert(FloatEqu(Obj1.sObject.sExtended, Obj2.sObject.sExtended));
    Assert(Obj1.sObject.sBoolean = Obj2.sObject.sBoolean);
    Assert(Obj1.sObject.sByteBool = Obj2.sObject.sByteBool);
    Assert(Obj1.sObject.sWordBool = Obj2.sObject.sWordBool);
    Assert(Obj1.sObject.sLongBool = Obj2.sObject.sLongBool);
    Assert(Obj1.sObject.sEnum = Obj2.sObject.sEnum);
    Assert(FloatEqu(Obj1.sObject.sDateTime, Obj2.sObject.sDateTime));
    Assert(FloatEqu(Obj1.sObject.sDate, Obj2.sObject.sDate));
    Assert(FloatEqu(Obj1.sObject.sTime, Obj2.sObject.sTime));

    Assert(Obj1.IntList.Count = Obj2.IntList.Count);
    for i := 0 to Obj1.IntList.Count - 1 do
      Assert(Obj1.IntList[i] = Obj2.IntList[i]);

    Assert(Obj1.StringList.Count = Obj2.StringList.Count);
    for i := 0 to Obj1.StringList.Count - 1 do
      Assert(Obj1.StringList[i] = Obj2.StringList[i]);
  end;
begin
  Test2List := TTest2List.Create;
  Test2List2 := TTest2List.Create;
  try
    for i := 0 to Random(5) do
    begin
      Test2 := TTest2.Create;
      FillTest2(Test2);
      Test2List.Add(Test2);
    end;
    Test2List.SaveToJsonString(s, False);
    //mmo1.Lines.Text := s;

    Test2List2.LoadFromJsonString(s);
    Test2List2.SaveToJsonString(s, False);
    //mmo2.Lines.Text := s;

    Assert(Test2List.Count = Test2List2.Count);
    for i := 0 to Test2List.Count - 1 do
    begin
      CheckTest2(Test2List[i], Test2List2[i]);
    end;
  finally
    Test2List.Free;
    Test2List2.Free;
  end;

  Test3 := TTest3.Create;
  Test32 := TTest3.Create;
  try
    Test3.sString := RandomStr;
    for i := 0 to Random(5) do
    begin
      Test2 := TTest2.Create;
      FillTest2(Test2);
      Test3.sList.Add(Test2);
    end;
    Test3.SaveToJsonString(s, False);
    mmo1.Lines.Text := s;

    Test32.LoadFromJsonString(s);
    Test32.SaveToJsonString(s, False);
    mmo2.Lines.Text := s;

    Assert(Test3.sString = Test32.sString);
    Assert(Test3.sList.Count = Test32.sList.Count);
    for i := 0 to Test3.sList.Count - 1 do
    begin
      CheckTest2(Test3.sList[i], Test32.sList[i]);
    end;
  finally
    Test3.Free;
    Test32.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  //CnJsonPersistentEnableDbgLog := True;
end;

end.
