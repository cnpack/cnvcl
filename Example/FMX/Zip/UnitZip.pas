unit UnitZip;

interface

uses
  Windows, Messages, SysUtils, Classes, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, CnZip, FMX.ComboEdit, FMX.Edit, FMX.Memo, FMX.TabControl, FMX.Types, System.Types, System.UITypes,
  FMX.ScrollBox, FMX.Controls.Presentation, Vcl.FileCtrl;

type
  TFormZip = class(TForm)
    PageControl1: TTabControl;
    tsReader: TTabItem;
    lblZip: TLabel;
    lblPassword: TLabel;
    edtZip: TEdit;
    btnBrowse: TButton;
    btnRead: TButton;
    mmoZip: TMemo;
    btnExtract: TButton;
    edtPassword: TEdit;
    tsWriter: TTabItem;
    lblPass: TLabel;
    mmoFiles: TMemo;
    btnCreate: TButton;
    btnAdd: TButton;
    btnSave: TButton;
    btnZipDir: TButton;
    chkRemovePath: TCheckBox;
    edtPass: TEdit;
    cbbMode: TComboEdit;
    btnRemoveFile: TButton;
    tsStream: TTabItem;
    btnTestStream: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    dlgOpenFile: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnReadClick(Sender: TObject);
    procedure btnExtractClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnZipDirClick(Sender: TObject);
    procedure btnRemoveFileClick(Sender: TObject);
    procedure btnTestStreamClick(Sender: TObject);
  private
    FWriter: TCnZipWriter;
  public

  end;

var
  FormZip: TFormZip;

implementation

{$R *.fmx}

uses
  CnBase64;

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
  mmoZip.Lines.Clear;
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
    mmoZip.Lines.Clear;
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
    FWriter.RemovePath := chkRemovePath.IsChecked;
    FWriter.Password := edtPass.Text;
    FWriter.CreateZipFile(dlgSave.FileName);
    FWriter.Comment := 'This is a Comment.';
    mmoFiles.Lines.Clear;
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
  cbbMode.ItemIndex := 1;
end;

procedure TFormZip.btnRemoveFileClick(Sender: TObject);
var
  S: string;
  Idx: Integer;
begin
  if FWriter = nil then
    Exit;

  if not InputQuery('Enter', 'Enter a File Index to Remove (0 Based):', S) then
    Exit;

  Idx := StrToInt(S);

  if (Idx >= 0) and (Idx < mmoFiles.Lines.Count) then
  begin
    if FWriter.RemoveFileByIndex(Idx) then
      mmoFiles.Lines.Delete(Idx);
//    S := mmoFiles.Lines[Idx];
//    if S <> '' then
//    begin
//      FWriter.RemoveFile(S);
//      mmoFiles.Lines.Delete(Idx);
//    end;
  end;

end;

procedure TFormZip.btnTestStreamClick(Sender: TObject);
const
  TEST_DATA =
    'eNrNWFtvG0UUfl4k/gPPaB3OmTlz2zcQPCIhkEA8Vqj9AbzwfyKVGiGhGmQnxHUdNXUKuTSuSxwppRAh' +
    'HhCgCIEiIrWVOLPjWTut4/haYku7M+vZc873ncuccdKodw62Krd+TZPq4XfPVhrf79RvrLXS5O7h1804' +
    '9vfaF9XrfrzZ2CnHcXgjzryk9aP2bpp0b6zWdsvV9TAKz1ZrzePOabudJvWTSqfbXT9Kk4MfG3/VjoP8' +
    '3e3mcV9DfxZ09Od+FN7a6rZPVx6kycqDO1t8/aV2vVtNk41W9ffKUbR648k3m7xyv3P6Q4Wt2a9t5xr+' +
    '6VTX/rz5NMiJs77k/UfNR288bNUfR/M2V4IgL3Lr2e3HN58GYQGah9Use8GV1YPlh3+niX9349+91fSV' +
    'RIBQJdAldK+ByyRlBGkihAODKHgIQgoAQMg/mCZvvo9GgXDaWgArUEjie5qkCfmFIDQ5LxQ0OmeNEU5I' +
    'rdNk74kn4/5yuxWupW+/7B6utbwH/Mu5bFgC8pP8wRJAvMolLznO0uQzf/nonbfOroq/Ft9Xz8BDyIAy' +
    'YUfDew+dVAqdRyDIakCNBTitJYkIziqS2hqJMATc9nKz/Dw0HBvacFzvvv1hbiSeD1C6zDMowLJnQCsJ' +
    'EsipITgjCgRpw5P+M4YISPyGQ5JCSLRKRIweVOXzeC/d/2nv57Fwijm5kEQmYUEId7frJxcjLAWIn169' +
    'NkxDZGAE6kkhK5cpPTpq+5YopzSvMYZQFnGrjOPE7K3RwpEkqYSce9xybPoKYCZDiBnIDOy0CBWvEhwG' +
    'pkDoFFpQPkBsdOooiD2HFhrYnWB7GvQYGgYdPkdapI910pPFOtcn6rGj3fkRz1WZnD0np3dO7vwxfU5P' +
    'DVZOnNgTgh2a3i+AfT4aOFUkBCUGxszw6Tig3OFyLA70Bx/zBFUwEwicNKj4N4nErKgX2ZDEWxUHtA9d' +
    'L3IBrg829bkZG7YMXcfLgD1REPTUFbFAvnCyNrJK6YVUeybDZiH+ZiPDhk6qIEMOkOFASyPN5YoBm6EZ' +
    'vQWMxotusE2TRQ/Kqc/1mntUPfftbgas0s2Clc60pNJGrCCsA7RG0eXB6hhuL5wZh2FM2hANgQxXruAn' +
    '5CunGkg4jlMuwMG5EVS71Tm995U/5MRRqXK73W78dutes8xx3PaP/VFuJExYcpNkqxZotHs9/wAaORyu' +
    'ggzt9K7lMwcMHKWM0EXXRsZwGhuLl8a1ihs39ZIq1WxlWyHkDbpkIxdUtpkMfzCbtX7lrSY3oIaKYyYp' +
    'chq4l1ETNLNDKEBvDTG3dgxFF/a0U/Aj7Ow1LzebjbSiX9+1sIobdqNn48dv7EDCEoyhaBH8+Ko4a+Hw' +
    'ZvMiqUxROLTQnE9GW5qNH9K+JNleCl2gaM78aP/XDaqx+IlG806ijPJe1chsoR3+N44A1NrvMf9jf/Af' +
    's11HJw==';
var
  Outs, Deds: TMemoryStream;
begin
  Outs := TMemoryStream.Create;
  Base64Decode(TEST_DATA, Outs);
  Outs.Position := 0;

  Deds := TMemoryStream.Create;
  CnZipUncompressStream(Outs, Deds);

  if Deds.Size > 0 then
  begin
    ShowMessage('UnCompress Bytes ' + IntToStr(Deds.Size));
    ShowMessage(PChar(Deds.Memory));  // 先不管尾部是否有 #0
  end;
  Deds.Free;
  Outs.Free;
end;

end.
