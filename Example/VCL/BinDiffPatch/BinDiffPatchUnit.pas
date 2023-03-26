unit BinDiffPatchUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, FileCtrl;

type
  TBinaryDiffPatchForm = class(TForm)
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    pgc1: TPageControl;
    tsFile: TTabSheet;
    grpDiff: TGroupBox;
    lblDiffOld: TLabel;
    lblDiffNew: TLabel;
    lblPatch: TLabel;
    lblPatchNew: TLabel;
    edtDiffOld: TEdit;
    edtDiffNew: TEdit;
    btnDiffBrowseOld: TButton;
    btnDiffBrowseNew: TButton;
    btnDiff: TButton;
    edtPatch: TEdit;
    btnDiffBrowsePatch: TButton;
    btnBinaryPatch: TButton;
    edtPatchNew: TEdit;
    btnPatchBrowseNew: TButton;
    tsDirectory: TTabSheet;
    grpDir: TGroupBox;
    lblOldDir: TLabel;
    edtOldDir: TEdit;
    edtNewDir: TEdit;
    lblNewDir: TLabel;
    edtPatchDir: TEdit;
    lblPatchDir: TLabel;
    btnBrowseOldDir: TButton;
    btnBrowseNewDir: TButton;
    btnBrowsePatchDir: TButton;
    btnDiffDir: TButton;
    edtNewOutDir: TEdit;
    lblOutDir: TLabel;
    btnBrowseOutDir: TButton;
    btnDirPatch: TButton;
    procedure btnDiffBrowseOldClick(Sender: TObject);
    procedure btnDiffBrowseNewClick(Sender: TObject);
    procedure btnDiffClick(Sender: TObject);
    procedure btnDiffBrowsePatchClick(Sender: TObject);
    procedure btnBinaryPatchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnBrowsePatchDirClick(Sender: TObject);
    procedure btnBrowseOldDirClick(Sender: TObject);
    procedure btnBrowseNewDirClick(Sender: TObject);
    procedure btnBrowseOutDirClick(Sender: TObject);
    procedure btnDiffDirClick(Sender: TObject);
    procedure btnDirPatchClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BinaryDiffPatchForm: TBinaryDiffPatchForm;

implementation

uses CnBinaryDiffPatch;

const
  SEL_DIR_CAPTION = 'Select a Directory:';

{$R *.DFM}

procedure TBinaryDiffPatchForm.btnDiffBrowseOldClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtDiffOld.Text := dlgOpen.FileName;
end;

procedure TBinaryDiffPatchForm.btnDiffBrowseNewClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtDiffNew.Text := dlgOpen.FileName;
end;

procedure TBinaryDiffPatchForm.btnDiffClick(Sender: TObject);
begin
  if Trim(edtPatch.Text) <> '' then
  begin
    if CnBinaryDiffPatch.BinaryDiffFile(edtDiffOld.Text, edtDiffNew.Text, Trim(edtPatch.Text)) then
      ShowMessage('Diff File Save to ' + Trim(edtPatch.Text))
    else
      ShowMessage('Diff File Fail !');
  end;
end;

procedure TBinaryDiffPatchForm.btnDiffBrowsePatchClick(Sender: TObject);
begin
  if dlgSave.Execute then
    edtPatch.Text := dlgSave.FileName;
end;

procedure TBinaryDiffPatchForm.btnBinaryPatchClick(Sender: TObject);
begin
  if Trim(edtPatch.Text) <> '' then
  begin
    if CnBinaryDiffPatch.BinaryPatchFile(edtDiffOld.Text, Trim(edtPatch.Text), edtPatchNew.Text) then
      ShowMessage('Restore File From Patch Save to ' + edtPatchNew.Text)
    else
      ShowMessage('Restore File From Patch Fail !');
  end;
end;

procedure TBinaryDiffPatchForm.FormCreate(Sender: TObject);
begin
  pgc1.ActivePageIndex := 0;
end;

procedure TBinaryDiffPatchForm.btnBrowsePatchDirClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := 'C:\';
  if SelectDirectory(SEL_DIR_CAPTION, 'C:\', Dir) then
    edtPatchDir.Text := Dir;
end;

procedure TBinaryDiffPatchForm.btnBrowseOldDirClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := 'C:\';
  if SelectDirectory(SEL_DIR_CAPTION, 'C:\', Dir) then
    edtOldDir.Text := Dir;
end;

procedure TBinaryDiffPatchForm.btnBrowseNewDirClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := 'C:\';
  if SelectDirectory(SEL_DIR_CAPTION, 'C:\', Dir) then
    edtNewDir.Text := Dir;
end;

procedure TBinaryDiffPatchForm.btnBrowseOutDirClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := 'C:\';
  if SelectDirectory(SEL_DIR_CAPTION, 'C:\', Dir) then
    edtNewOutDir.Text := Dir;
end;

procedure TBinaryDiffPatchForm.btnDiffDirClick(Sender: TObject);
begin
  if CnBinaryDiffPatch.BinaryDiffDirectory(edtOldDir.Text, edtNewDir.Text, edtPatchDir.Text) then
    ShowMessage('Diff Directory Files Save to ' + Trim(edtPatchDir.Text))
  else
    ShowMessage('Diff Directory Fail !');
end;

procedure TBinaryDiffPatchForm.btnDirPatchClick(Sender: TObject);
begin
  if CnBinaryDiffPatch.BinaryPatchDirectory(edtOldDir.Text, edtPatchDir.Text, edtNewOutDir.Text) then
    ShowMessage('Restore Directory From Patches Save to ' + edtNewOutDir.Text)
  else
    ShowMessage('Restore Directory From Patches Fail !');
end;

end.
