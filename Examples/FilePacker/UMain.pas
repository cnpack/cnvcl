unit UMain;

interface

//{$DEFINE ZIP}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnFilePacker, ShellAPI, ShlObj, ComCtrls, CnCommon
  {Uzip 是实现了ICnCompress压缩接口，
用了ZLIBEX.PAS这个第三方压缩单元库!
cnpack不带zlibex.pas，使用条件编译引用uzip，
即使没有zlibex库也可以顺利编译}
{$IFDEF ZIP}
, uzip
{$ENDIF}
;

type
  TForm1 = class(TForm)
    btnAddDirectory: TButton;
    Addfile: TButton;
    btnUnpacking: TButton;
    Button5: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    CheckBox1: TCheckBox;
    tv1: TTreeView;
    lv1: TListView;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnUnpackingClick(Sender: TObject);
    procedure AddfileClick(Sender: TObject);
    procedure btnAddDirectoryClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tv1Change(Sender: TObject; Node: TTreeNode);
    procedure lv1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
    procedure CreateFileAndDirectoryTree;
    procedure Packing;  
    //procedure GetFileInfo;
    //procedure GetDirectoryInfo;
    procedure EnabledAllButton;
    procedure DisEnabledAllButton;  
    { Private declarations }
  public
    { Public declarations }
  end;

type
  TItem = class
  private
    FPInfo: TPackFileInformation;
  end;

var
  Form1: TForm1;
  fp: TCnFilePacker;
  GItem: TItem;

implementation

{$R *.dfm}

function ShowDirectoryDialog: string;
var
  TitleName: string;
  lpItemID: PItemIDList;
  BrowseInfo: TBrowseInfo;
  DisplayName: array[0..MAX_PATH] of char;
  TempPath: array[0..MAX_PATH] of char;
begin
//使用sh级函数显示选择文件夹对话框
  TempPath := ''; 
  FillChar(BrowseInfo, sizeof(TBrowseInfo), #0);
  BrowseInfo.hwndOwner := Form1.Handle;
  BrowseInfo.pszDisplayName := @DisplayName;
  TitleName := 'Please specify a directory';
  BrowseInfo.lpszTitle := PChar(TitleName);
  BrowseInfo.ulFlags := BIF_RETURNONLYFSDIRS;
  lpItemID := SHBrowseForFolder(BrowseInfo);
  if lpItemId <> nil then
    SHGetPathFromIDList(lpItemID, TempPath);
  GlobalFreePtr(lpItemID);
  Result := TempPath;
end;

procedure TForm1.btnAddDirectoryClick(Sender: TObject);
var
  Path: string;
begin
  Path := ShowDirectoryDialog;
  if Path = '' then
    exit;
  fp.AddDircetory(Path);
  Self.packing;
  CreateFileAndDirectoryTree;
end;

procedure TForm1.AddfileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    if OpenDialog1.FileName <> '' then
    begin
      fp.AddFile(OpenDialog1.FileName);
      Self.packing;
      CreateFileAndDirectoryTree;
    end;
end;

procedure TForm1.btnUnpackingClick(Sender: TObject);
var
  Path: string;
begin
  Path := ShowDirectoryDialog;
  if Path = '' then
    Exit;

  fp.SavePath := Path;
  DisEnabledAllButton;
  Cursor := crHourGlass;
  fp.SaveToFiles;
  
  ShowMessage('UnPacking End!');
  EnabledAllButton;
  Cursor := crDefault;
  button5.Enabled := False;
  btnUnpacking.Enabled := False;
  Button1.Enabled := False;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Path: string;
begin
 Path := ShowDirectoryDialog;
  if Path = '' then
    Exit;
  fp.SavePath := Path;
  DisEnabledAllButton;
  Cursor := crHourGlass;
  fp.SaveToFile(GItem.FPInfo);

  ShowMessage('UnPacking End!');
  EnabledAllButton;
  Cursor := crDefault;
  button5.Enabled := False;
  btnUnpacking.Enabled := False;
  Button1.Enabled := False;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    if SaveDialog1.FileName <> '' then
    begin
      Form1.Caption := SaveDialog1.FileName;
      fp.DestFileName := SaveDialog1.FileName;
      EnabledAllButton;
      Button5.Enabled := false;
      btnUnpacking.Enabled := False;
      Button1.Enabled := False;
      if FileExists(SaveDialog1.FileName) then
        CreateFileAndDirectoryTree;
    end;
end;

procedure TForm1.CreateFileAndDirectoryTree;
var
  CurrentDirectory, LastNameofCurrentDirectory: string;

  procedure FindNode(AParent: TTreeNode; ADirName: string);
  var
    srec: TSearchRec;
    tmpLastParent: TTreeNode;
    tmpCurrentDirectory, tmpLastNameofCurrentDirectory: string;      //保存当前目录层递归没有退栈，nnd
  begin
    if FindFirst(ADirName, faDirectory, srec) = 0 then
      repeat
        if (srec.Name = '.') or (srec.Name = '..') then
          Continue;
        tmpLastParent := AParent;
        AParent := tv1.Items.AddChild(AParent, srec.Name);
        tmpLastNameofCurrentDirectory := LastNameofCurrentDirectory;
        tmpCurrentDirectory := CurrentDirectory;
        LastNameofCurrentDirectory := LastNameofCurrentDirectory + srec.Name + '\';
        CurrentDirectory := CurrentDirectory + srec.Name + '\';
        FindNode(AParent, copy(ADirName, 1, Length(ADirName) - 3) + srec.Name + '\*.*');
        LastNameofCurrentDirectory := tmpLastNameofCurrentDirectory;
        CurrentDirectory := tmpCurrentDirectory;
        AParent := tmpLastParent;
      until FindNext(srec) <> 0;
  end;

var
  Ap: TArrayPackFileInformation;
  G_Parent: TTreeNode;
  i, RandomI: Integer;
  TempPath: PChar;
begin
  Randomize;
  RandomI := Random(100000);
  GetMem(TempPath, 256);
  if GetTempPath(256, TempPath) = 0 then
    Exit;
  ap := fp.PackFileDirectoryInfo;
  for I := 0 to Length(ap) - 1 do
    ForceDirectories(TempPath + 'CnFilePacker\' + IntToStr(RandomI) + '\' + ap[i].Name);
  tv1.Items.Clear;
  G_Parent := tv1.Items.AddChild(nil, ExtractFileName(Form1.Caption));
  FindNode(G_parent, TempPath + 'CnFilePacker\' + IntToStr(RandomI) + '\*.*');
end;

procedure TForm1.DisEnabledAllButton;
var
  i: Integer;
begin
  for I := 0 to ComponentCount - 1 do
    if Components[i] is Tbutton then
      (Components[i] as Tbutton).Enabled := false;
end;

procedure TForm1.EnabledAllButton;
var
  I: Integer;
begin
  for I := 0 to ComponentCount - 1 do
    if Components[i] is Tbutton then
      (Components[i] as Tbutton).Enabled := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  fp := TCnFilePacker.Create(nil);
  DisEnabledAllButton;
  Button5.Enabled := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  fp.Free;
  fp := nil;
end;

procedure TForm1.Packing;
begin
  if CheckBox1.Checked then
  begin
    {$IFDEF ZIP}
    fp.AddCompressClass(TCnZipCompress);
    fp.CompressMode := cmZIP;  
    {$ENDIF}
  end;
  DisEnabledAllButton;
  Cursor := crHourGlass;
  fp.DoPack;
  ShowMessage('Packing end!');
  EnabledAllButton;
  Cursor := crDefault;
  button5.Enabled := False;
end;

procedure TForm1.tv1Change(Sender: TObject; Node: TTreeNode);
var
  AText: string;  //路径
  Ap: TArrayPackFileInformation;
  i: Integer;
  FItem: TItem;
  ListItem: TListItem;
begin
  AText := '';
  while Node <> nil do
  begin
    AText := Node.Text + '\' + AText;
    Node := Node.Parent;
  end;
  Delete(AText, 1, Length(ExtractFileName(Form1.Caption)) + 1);
  Ap := fp.PackFileInformation;

  lv1.Items.Clear;
  for I := 0 to Length(ap) - 1 do
  begin
    if (AText = '') or (IncludeTrailingBackslash(ExtractFilePath(Ap[i].Name)) = AText) then
    begin
      FItem := TItem.Create;
      StrCopy(FItem.FPInfo.Name, PAnsiChar(ExtractFileName(Ap[i].Name)));
      FItem.FPInfo.DataStart := Ap[i].DataStart;
      ListItem := lv1.Items.Add;
      ListItem.Caption := ExtractFileName(Ap[i].Name);
      ListItem.Data := FItem;
    end;
  end;
  btnUnpacking.Enabled := True;
  Button1.Enabled := True;
end;

procedure TForm1.lv1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if (Change = ctState) and (Item <> nil) then
  begin
    GItem := TItem(Item.Data);
    btnUnpacking.Enabled := True;
    Button1.Enabled := True;
  end;
end;

end.

