unit w_XMLPersistentTestCase;

interface

uses TestFramework, CnXMLPersistent, Classes, TypInfo, XMLDoc,
  w_PersistentClassSample, Forms, Graphics, w_PivotUnit, w_StreamUnitTest,
  w_frmTestcase, Variants;

type
  TTestType = (Test1, Test2, Test3);

  TTypeTest = packed record
    name: string;
    Size: Integer;
    isTrue: Boolean;
  end;

  TXMLObjectWriterTestCase = class(TTestCase)
  private
  public

  private
    Frm: TfrmTest;
    FWriter: TMyCustomWriter;
    procedure FillPivotField(Index: Integer; tbl: TPivotTable);
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure WritePicture;
    procedure WriteCollectionToXMLTest;
    procedure WriteFormToXMLTest;
    procedure WriteValueTest;
    procedure WriteStream;
    procedure WriteTStrings;
  end;

  TXMLObjectBuilderTestCase = class(TTestCase)
  private
    FReader: TMycustomReader;
    Frm: TfrmTest;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure BuildCollectionXMLFileTest;
    procedure BuildFormXMLFileTest;
    procedure ReadPicture;
    procedure ReadStream;
    procedure ReadTStrings;
    procedure ReadValue;
  end;

  TDynamicBuildTestCase = class(TTestCase)
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure BuildCollectionItemTest;
    procedure BuildCollectionTest;
    procedure BuildComponentTest;
    procedure BuildPersistentTest;
  end;

function GetAppPath: string;

implementation

uses SysUtils, Dialogs, CnDynObjBuilder, ExtCtrls, StdCtrls;

function GetAppPath: string;
begin
  result := ExtractFilePath(Application.ExeName);
end;

procedure TXMLObjectWriterTestCase.FillPivotField(Index: Integer; tbl:
  TPivotTable);
var
  itm: TPivotItem;
begin
  tbl.PivotField[index].AutoShowCount := 25 + Index;
  tbl.PivotField[index].BlankLine := true;
  tbl.PivotField[index].Calculation := 23;
  itm := TPivotItem.Create(tbl.PivotField[Index].PivotItems);
  itm.ItemName := 'itm' + intToStr(Index);
  itm.Visible := true;
end;

procedure TXMLObjectWriterTestCase.WritePicture;
var
  pic: TImage;
begin
  pic := TImage.Create(frm);
  try
    pic.Stretch := true;
    pic.Transparent := false;
    pic.AutoSize := false;
    Pic.Picture.LoadFromFile(GetAppPath + 'test.bmp');

    FWriter.WriteObjectToXML('PictureObject', Pic);

    FWriter.SaveXMLFile(GetAppPath + 'TImage.xml');
  finally
    FreeAndNil(Pic);
  end;
end;

procedure TXMLObjectWriterTestCase.Setup;
begin
  inherited;
  FWriter := TMyCustomWriter.Create(nil);
  frm := TfrmTest.Create(nil);
  FWriter.LoadFromXMLString('');
end;

procedure TXMLObjectWriterTestCase.TearDown;
begin
  FreeAndNil(Frm);
  FreeAndNil(FWriter);
  inherited;
end;

procedure TXMLObjectWriterTestCase.WriteCollectionToXMLTest;
var
  tbl: TPivotTable;
begin

  tbl := TPivotTable.Create(nil);
  try
    tbl.TableName := 'test';
    tbl.PivotFields.Add;
    tbl.PivotFields.Add;
    tbl.PivotFields.Add;
    FillPIvotField(0, tbl);
    FillPIvotField(1, tbl);
    FillPIvotField(2, tbl);
    FWriter.WriteObjectToXML('PivotTableObject', tbl);
    FWriter.SaveXMLFile(GetAppPath + 'PivotTable.xml');
  finally
    tbl.Free;
  end;

end;

procedure TXMLObjectWriterTestCase.WriteFormToXMLTest;

begin
  FWRiter.WriteObjectToXML('FormObject', Frm);
  FWriter.SaveXMLFile(GetAppPath + 'Form.xml');
end;

procedure TXMLObjectWriterTestCase.WriteValueTest;

var
  intVal: Integer;
  strVal: string;
  BooleanVal: boolean;
  t1: TTestType;
begin
  intVal := 255;
  strVal := 'Test';
  BooleanVal := true;
  t1 := test1;
  FWriter.WriteValueToXML('IntegerValue', intVal);
  FWriter.WriteValueToXML('StringValue', strVal);
  FWriter.WriteValueToXML('BooleanValue', BooleanVal);
  FWriter.WriteValueToXML('TestValue', T1);
  self.FWriter.SaveXMLFile(GetAppPath + 'Record.xml');

end;

procedure TXMLObjectWriterTestCase.WriteStream;
var
  S: TStreamOwner;
  lst: TStringList;
begin
  S := TStreamOwner.Create;
  lst := TStringList.Create;
  try
    lst.LoadFromFile(GetAppPath + 'TImage.xml');
    S.Img.Picture.LoadFromFile(GetAppPath + 'test.bmp');
    lst.SaveToStream(S.Stream);
    FWriter.WriteObjectToXML('StreamObject', S);
    FWriter.SaveXMLFile(GetAppPath + 'TStreamOwner.xml');
  finally
    FreeAndNil(S);
    FreeAndNil(lst);
  end;

end;

procedure TXMLObjectWriterTestCase.WriteTStrings;
var
  lst: TStringList;
begin
  lst := TStringList.Create();
  try
    lst.Add('Test 1');
    lst.Add('Test 2');
    lst.Add('Test 3');
    FWriter.WriteObjectToXML('StringListObject', lst);
    FWriter.SaveXMLFile(GetAppPath + 'TMemo.xml');
  finally
    FreeAndNil(lst);
  end;

end;

procedure TXMLObjectBuilderTestCase.BuildCollectionXMLFileTest;
var
  tbl: TPivotTable;
begin
  RegisterClass(TPivotTable);
  RegisterClass(TPivotField);
  RegisterClass(TCollection);
  RegisterClass(TPivotItem);
  tbl := TPivotTable.Create(nil);
  try
    FReader.LoadXMLFormFile(GetAppPath + 'PivotTable.xml');
    FReader.ReadXmlToObject('PivotTableObject', tbl);
    self.CheckEqualsString(tbl.TableName, 'test', ' table name is error !');
    CheckEquals(tbl.PivotField[2].AutoShowCount, 27, ' AutoShowCount is Error !');
    CheckEqualsString(tbl.PivotField[2].PivotItem[0].ItemName, 'itm2', 'Pivot Item Name is error!');

  finally
    tbl.Free;
  end;

end;

procedure TXMLObjectBuilderTestCase.BuildFormXMLFileTest;
var
  obj: TForm;
begin

  RegisterClass(TForm);
  obj := TForm.Create(nil);
  try
    FReader.LoadXMLFormFile(GetAppPath + 'Test.xml');
    FReader.ReadXmlToObject('FormObject', Obj);
  finally
    obj.Free;
  end;
end;

procedure TXMLObjectBuilderTestCase.ReadPicture;
var
  Img: TImage;
begin
  Img := TImage.Create(frm);
  Img.Parent := frm;
  Img.Left := 1;
  img.Top := 1;
  FReader.LoadXMLFormFile(GetAppPath + 'TImage.xml');
  FReader.ReadXmlToObject('PictureObject', Img);
  img.Refresh;
  Frm.ShowModal;
end;

procedure TXMLObjectBuilderTestCase.ReadStream;
var
  S: TStreamOwner;
  str: TFileStream;
begin
  Str := TFilestream.Create(GetAppPath + 'TImage.xml', fmOpenReadWrite);
  S := TStreamOwner.Create;
  try
    FReader.LoadXMLFormFile(GetAppPath + 'TStreamOwner.xml');
    FREader.ReadXmlToObject('StreamObject', S);
    S.Stream.Position := 0;
    Str.CopyFrom(S.Stream, s.Stream.Size);
  finally
    FreeAndNil(str);
    FreeAndNil(S);
  end;
end;

procedure TXMLObjectBuilderTestCase.ReadTStrings;
var
  str: TStringList;
begin
  str := TStringList.Create;
  try
    FReader.LoadXMLFormFile(GetAppPath + 'TMemo.xml');
    FReader.ReadXmlToObject('StringListObject', str);
    self.CheckEquals(str.Count, 3, ' Count is error !');
    CheckEquals(str.Strings[0], 'Test 1');
  finally
    FreeAndNil(str)
  end;
end;

procedure TXMLObjectBuilderTestCase.ReadValue;
var
  val: Variant;
  T1: TTestType;
begin
  FReader.LoadXMLFormFile(GetAppPath + 'Record.xml');
  Val := FReader.ReadXMLToValue('TestValue');
  T1 := Test1;
  Check(val = T1, '');
  Val := FReader.ReadXMLToValue('IntegerValue');
  check(val = 255, '');
  val := FReader.ReadXMLToValue('StringValue');
  check(val = 'Test', '');
  Val := FReader.ReadXMLToValue('BooleanValue');
  check(val = true, '');

end;

procedure TXMLObjectBuilderTestCase.Setup;

begin
  inherited;
  FReader := TMyCustomReader.Create(nil);
  Frm := TFrmTest.Create(nil);
end;

procedure TXMLObjectBuilderTestCase.TearDown;
begin
  FreeAndNil(Frm);
  FreeAndNil(FReader);

  inherited;
end;

procedure TDynamicBuildTestCase.BuildCollectionItemTest;

var
  Coll: TCollection;
  itm: TCollectionItem;
begin
  RegisterClass(TCollection);
  RegisterClass(TPivotTable);

  Coll := TCnDynamicBuilder.BuildCollection('TCollection', TPivotTable);
  itm := TCnDynamicBuilder.BuildCollectionItem('TPivotTable', Coll);
  checkTrue(itm is TPivotTable, ' itm is not TPivotTable!');

  itm.Free;
  Coll.Free;

end;

procedure TDynamicBuildTestCase.BuildCollectionTest;
var
  collection: TCollection;
begin
  RegisterClass(TCollection);
  Collection := TCnDynamicBuilder.BuildCollection('TCollection', TPivotTable);
  //check
  CheckTrue(Collection is TCollection, ' Collection is not TPivotTables');
  Collection.Free;
end;

procedure TDynamicBuildTestCase.BuildComponentTest;

var
  comp: TComponent;
begin
//
//  TCnDynamicBuilder.BuildComponent()
  RegisterClass(TTimer);
  comp := TCnDynamicBuilder.BuildComponent('TTimer');
  try
    self.CheckTrue(Comp is TTimer, ' Component is not TTimer');
  finally
    FreeAndNil(Comp);
  end;
end;

procedure TDynamicBuildTestCase.BuildPersistentTest;
var
  lst: TPersistent;
begin
  RegisterClass(TStringList);
  lst := TCnDynamicBuilder.BuildPersistent('TStringList');
  if lst is TStringList then
  begin
    TStringList(lst).Text := 'aaa';
    self.CheckEqualsString(trim(TStringList(lst).Text), 'aaa', ' string is error!');
  end
  else
  begin
    self.Check(false, 'lst is not TStringList builder error!');
  end;
  lst.Free;
end;

procedure TDynamicBuildTestCase.Setup;
begin
  inherited;
end;

procedure TDynamicBuildTestCase.TearDown;
begin
  inherited;
end;

initialization
  TestFramework.RegisterTest(TXMLObjectWriterTestCase.Suite);
  TestFramework.RegisterTest(TXMLObjectBuilderTestCase.Suite);
  TestFramework.RegisterTest(TDynamicBuildTestCase.Suite);
end.
