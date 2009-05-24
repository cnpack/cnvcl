unit fMainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, CnHashTable, Buttons;

type
  TForm3 = class(TForm)
    btnGenerateHashTable1: TButton;
    mmoLog: TMemo;
    btnGenerateHashTable2: TButton;
    btnGenerateHashTable3: TButton;
    btnCleanLog: TButton;
    edtSourceFile: TEdit;
    rgHashType: TRadioGroup;
    edtInitialBuckets: TEdit;
    Label1: TLabel;
    btnSearch: TButton;
    edtSearch: TEdit;
    edtSearchRepeat: TEdit;
    Label2: TLabel;
    btnTravel: TButton;
    edtTravelFile: TEdit;
    Label3: TLabel;
    edtRehashPoint: TEdit;
    btnHashTableInfo: TSpeedButton;
    btnTravelSorted: TButton;
    procedure btnTravelSortedClick(Sender: TObject);
    procedure rgHashTypeClick(Sender: TObject);
    procedure btnHashTableInfoClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnTravelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnCleanLogClick(Sender: TObject);
    procedure btnGenerateHashTable1Click(Sender: TObject);
  private
    { Private declarations }
    ht: TCnHashTable;
    OldCaption: string;

    function GetHashTableInfo: string;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.btnGenerateHashTable1Click(Sender: TObject);
const
  csStartInfos: array[1..3] of string =
  ('Start test: Always sorted',
    'Start test: Not sorted',
    'Start test: Copy then sort');
var
  f: TextFile;
  s: string;
  t: DWORD;
  i: Integer;
  iLoop: Integer;
begin
  FreeAndNil(ht);
  try
    case rgHashType.ItemIndex of
      0: ht := TCnHashTableSmall.Create;
      1: ht := TCnHashTableMedium.Create;
      2: ht := TCnHashTableBig.Create;
      3:
        begin
          ht := TCnHashTable.Create(StrToInt(edtInitialBuckets.Text));
          ht.AutoRehashPoint := StrToInt(edtRehashPoint.Text);
        end;
    else
      raise Exception.Create('You must select a hash type.');
    end;
  except
    FreeAndNil(ht);
    raise;
  end;
  mmoLog.Lines.Add('');
  mmoLog.Lines.Add(csStartInfos[TButton(Sender).Tag]);
  try
    AssignFile(f, edtSourceFile.Text);
    Reset(f);

    if TButton(Sender).Tag > 1 then
      ht.BeginUpdate;
    t := GetTickCount;

    i := 0;
    while not Eof(f) do
    begin
      ReadLn(f, s);
// Add is insert only
//      if not ht.Exists(s) then
//        ht.Add(s, nil);
// Put is update or insert
      ht.Put(s, nil);
      Inc(i);
    end;

    mmoLog.Lines.Add('Copy: ' + IntToStr(i) + 'Lines, used ' + IntToStr(GetTickCount - t) + ' ms');

    if TButton(Sender).Tag > 2 then
    begin
      t := GetTickCount;
      ht.EndUpdate;
      mmoLog.Lines.Add('Sort: ' + IntToStr(GetTickCount - t) + 'ms');
    end;

    if TButton(Sender).Tag <> 2 then
    begin
      s := edtSearch.Text;
      t := GetTickCount;
      for i := 1 to 1000000 do
      begin
        ht.Exists(s);
      end;
      mmoLog.Lines.Add('Query (' + s + '): ' + IntToStr(GetTickCount - t) + ' ms/1,000,000');
    end;

    iLoop := ht.Count;
    if iLoop > 1000000 then
      iLoop := 1000000;
    t := GetTickCount;
    for i := 0 to iLoop - 1 do
    begin
      s := ht.Keys[i];
      Assert(ht.ExistsPos(s) = i, IntToStr(i));
    end;
    mmoLog.Lines.Add('Query Keys: ' + IntToStr(GetTickCount - t) + ' ms/' + IntToStr(iLoop));
  finally
    CloseFile(f);
  end;
  mmoLog.Lines.Add('End test');
  mmoLog.Lines.Add('');
end;

procedure TForm3.btnCleanLogClick(Sender: TObject);
begin
  mmoLog.Clear;
end;

procedure TForm3.btnSearchClick(Sender: TObject);
var
  i, iLoop: Integer;
  t: DWORD;
  s: string;
begin
  if Assigned(ht) then
  begin
    iLoop := StrToInt(edtSearchRepeat.Text);
    if iLoop <= 1 then
      Exit;
    s := edtSearch.Text;
    t := GetTickCount;
    for i := 1 to iLoop do
      ht.Exists(s);
    t := GetTickCount - t;
    if ht.Exists(s) then
      mmoLog.Lines.Add('Search (' + s + ') found: ' + IntToStr(t) + ' ms/' + edtSearchRepeat.Text)
    else
      mmoLog.Lines.Add('Search (' + s + ') not found: ' + IntToStr(t) + ' ms/' + edtSearchRepeat.Text);
  end;
end;

procedure TForm3.btnTravelClick(Sender: TObject);
var
  i, iLoop: Integer;
  t: DWORD;
  f: TextFile;
begin
  iLoop := ht.Count;
  AssignFile(f, edtTravelFile.Text);
  try
    ReWrite(f);
    t := GetTickCount;
    for i := 0 to iLoop - 1 do
    begin
      Writeln(f, ht.Keys[i]);
    end;
    mmoLog.Lines.Add('Travelled done: ' + IntToStr(GetTickCount - t) + ' ms/' + IntToStr(iLoop));
  finally
    CloseFile(f);
  end;
end;

procedure TForm3.btnTravelSortedClick(Sender: TObject);
var
  i: Integer;
  t: DWORD;
  f: TextFile;
begin
  AssignFile(f, edtTravelFile.Text);
  try
    ReWrite(f);
    t := GetTickCount;
    ht.BuildSortedList;
    mmoLog.Lines.Add('GetSortedList: ' + IntToStr(GetTickCount - t) + ' ms/' + IntToStr(ht.Count));
    with ht.SortedList do
    begin
      t := GetTickCount;
      for i := 0 to Count - 1 do
      begin
        Writeln(f, Strings[i]);
      end;
      mmoLog.Lines.Add('Travelled done: ' + IntToStr(GetTickCount - t) + ' ms/' + IntToStr(Count));
    end;
  finally
    CloseFile(f);
  end;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  OldCaption := Caption;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ht);
end;

function TForm3.GetHashTableInfo: string;
begin
  if not Assigned(ht) then
  begin
    Result := 'HashTable not created yet';
  end
  else if ht.ClassType = TCnHashTableSmall then
  begin
    Result := 'Small:(' + ht.Info + ')';
  end
  else if ht.ClassType = TCnHashTableMedium then
  begin
    Result := 'Medium:(' + ht.Info + ')';
  end
  else if ht.ClassType = TCnHashTableBig then
  begin
    Result := 'Big:(' + ht.Info + ')';
  end
  else if ht.ClassType = TCnHashTable then
  begin
    Result := 'Custom:(' + ht.Info + ')';
  end;
end;

procedure TForm3.rgHashTypeClick(Sender: TObject);
begin
  edtInitialBuckets.Enabled := rgHashType.ItemIndex = rgHashType.Items.Count - 1;
  edtRehashPoint.Enabled := edtInitialBuckets.Enabled;
end;

procedure TForm3.btnHashTableInfoClick(Sender: TObject);
begin
  mmoLog.Lines.Add(GetHashTableInfo);
end;

end.

