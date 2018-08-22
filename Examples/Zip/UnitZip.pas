unit UnitZip;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl, CnZip, ComCtrls;

type
  TFormZip = class(TForm)
    PageControl1: TPageControl;
    tsReader: TTabSheet;
    lblZip: TLabel;
    edtZip: TEdit;
    btnBrowse: TButton;
    btnRead: TButton;
    mmoZip: TMemo;
    dlgOpen: TOpenDialog;
    btnExtract: TButton;
    tsWriter: TTabSheet;
    mmoFiles: TMemo;
    btnCreate: TButton;
    btnAdd: TButton;
    btnSave: TButton;
    dlgSave: TSaveDialog;
    dlgOpenFile: TOpenDialog;
    btnZipDir: TButton;
    chkRemovePath: TCheckBox;
    edtPassword: TEdit;
    lblPassword: TLabel;
    lblPass: TLabel;
    edtPass: TEdit;
    cbbMode: TComboBox;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnReadClick(Sender: TObject);
    procedure btnExtractClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnZipDirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FWriter: TCnZipWriter;
  public
    { Public declarations }
  end;

var
  FormZip: TFormZip;

implementation

{$R *.DFM}

procedure TFormZip.btnBrowseClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtZip.Text := dlgOpen.FileName;
end;

procedure TFormZip.btnReadClick(Sender: TObject);
var
  ZR: TCnZipReader;
  I: Integer;
  Header: PCnZipHeader;
begin
  mmoZip.Clear;
  if CnZipFileIsValid(edtZip.Text) then
  begin
    mmoZip.Lines.Add('Is a Zip File.');
    ZR := TCnZipReader.Create;
    ZR.OpenZipFile(edtZip.Text);
    for I := 0 to ZR.FileCount - 1 do
    begin
      Header := ZR.FileInfo[I];
      mmoZip.Lines.Add(ZR.FileName[I] + ' in Central Directory.');

      mmoZip.Lines.Add(Format('  RequiredVersion: %4.4d', [Header^.RequiredVersion]));
      mmoZip.Lines.Add(Format('  Flag: $%4.4x', [Header^.Flag]));
      mmoZip.Lines.Add(Format('  CompressionMethod: %4.4d', [Header^.CompressionMethod]));
      mmoZip.Lines.Add(Format('  ModifiedDateTime: $%8.8x', [Header^.ModifiedDateTime]));
      mmoZip.Lines.Add(Format('  CRC32: $%8.8x', [Header^.CRC32]));
      mmoZip.Lines.Add(Format('  CompressedSize: %d', [Header^.CompressedSize]));
      mmoZip.Lines.Add(Format('  UncompressedSize: %d', [Header^.UncompressedSize]));
      mmoZip.Lines.Add(Format('  FileNameLength: %d', [Header^.FileNameLength]));
      mmoZip.Lines.Add(Format('  ExtraFieldLength: %d', [Header^.ExtraFieldLength]));
      mmoZip.Lines.Add(Format('  FileCommentLength: %d', [Header^.FileCommentLength]));
      mmoZip.Lines.Add(Format('  DiskNumberStart: %d', [Header^.DiskNumberStart]));
      mmoZip.Lines.Add(Format('  InternalAttributes: %d', [Header^.InternalAttributes]));
      mmoZip.Lines.Add(Format('  ExternalAttributes: %d', [Header^.ExternalAttributes]));
      mmoZip.Lines.Add(Format('  LocalHeaderOffset:  %8.8x', [Header^.LocalHeaderOffset]));

    end;

    mmoZip.Lines.Add('');
    mmoZip.Lines.Add(ZR.Comment);
    ZR.Free;
  end
  else
    mmoZip.Lines.Add('NOT a Zip File.');
end;

procedure TFormZip.btnExtractClick(Sender: TObject);
var
  ZR: TCnZipReader;
  Dir: string;
begin
  Dir := 'C:\';
  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 1000) then
  begin
    mmoZip.Clear;
    ZR := TCnZipReader.Create;
    ZR.OpenZipFile(edtZip.Text);
    ZR.Password := edtPassword.Text;
    ZR.ExtractAllTo(Dir);
    ZR.Free;
    ShowMessage('Extract OK.');
  end;
end;

procedure TFormZip.btnCreateClick(Sender: TObject);
begin
  FreeAndNil(FWriter);
  if dlgSave.Execute then
  begin
    FWriter := TCnZipWriter.Create;
    FWriter.RemovePath := chkRemovePath.Checked;
    FWriter.Password := edtPass.Text;
    FWriter.CreateZipFile(dlgSave.FileName);
    FWriter.Comment := 'This is a Comment.';
    mmoFiles.Clear;
  end;
end;

procedure TFormZip.btnAddClick(Sender: TObject);
begin
  if FWriter <> nil then
  begin
    if dlgOpenFile.Execute then
    begin
      if cbbMode.ItemIndex = 0 then
        FWriter.AddFile(dlgOpenFile.FileName, '', zcStored)
      else
        FWriter.AddFile(dlgOpenFile.FileName);
      mmoFiles.Lines.Add(dlgOpenFile.FileName);
    end;
  end;
end;

procedure TFormZip.btnSaveClick(Sender: TObject);
begin
  if FWriter <> nil then
  begin
    FWriter.Save;
    FWriter.Close;
    FreeAndNil(FWriter);
    ShowMessage('Zip file Save OK.');
  end;
end;

procedure TFormZip.btnZipDirClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := 'C:\';
  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 1000) then
    if dlgSave.Execute then
      if CnZipDirectory(Dir, dlgSave.FileName, zcStored, edtPass.Text) then
        ShowMessage('Zip Directory OK.');
end;

procedure TFormZip.FormCreate(Sender: TObject);
begin
  cbbMode.ItemIndex := 0;
end;

end.
