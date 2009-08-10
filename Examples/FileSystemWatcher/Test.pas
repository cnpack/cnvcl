unit Test;

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl, CnFileSystemWatcher, CnClasses;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    CheckBox1: TCheckBox;
    DriveComboBox1: TDriveComboBox;
    DirectoryListBox1: TDirectoryListBox;
    lbl1: TLabel;
    FileSystemWatcher1: TCnFileSystemWatcher;
    CheckBox2: TCheckBox;
    GroupBox1: TGroupBox;
    Memo2: TMemo;
    Button1: TButton;
    GroupBox2: TGroupBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    Button2: TButton;
    procedure CheckBox1Click(Sender: TObject);
    procedure DirectoryListBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CnFileSystemWatcherChange(Sender: TObject; FileOperation: TFileOperation;
      const FileName1, FileName2: String);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  FileSystemWatcher1.Active := CheckBox1.Checked;
end;

procedure TForm1.DirectoryListBox1Change(Sender: TObject);
begin
  FileSystemWatcher1.WatchedDir := DirectoryListBox1.Directory;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FileSystemWatcher1.WatchedDir := DirectoryListBox1.Directory;
end;

procedure TForm1.CnFileSystemWatcherChange(Sender: TObject; FileOperation: TFileOperation;
  const FileName1, FileName2: String);
begin
  case FileOperation of
    foAdded :    Memo1.Lines.Add('Added:    ' + FileName1 + ';   ' + FileName2);
    foRemoved :  Memo1.Lines.Add('Removed:  ' + FileName1 + ';   ' + FileName2);
    foModified : Memo1.Lines.Add('Modified: ' + FileName1 + ';   ' + FileName2);
    foRenamed :  Memo1.Lines.Add('Renamed:  ' + FileName1 + ';   ' + FileName2);
  end; 
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  FileSystemWatcher1.IncludePath := CheckBox2.Checked;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FileSystemWatcher1.FileMasks.Assign(Memo2.Lines);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  tmpnf: TNotifyFilters;
begin
  tmpnf := [];
  if CheckBox3.Checked then
    Include(tmpnf, nfFileNameChange);
  if CheckBox4.Checked then
    Include(tmpnf, nfDirNameChange);
  if CheckBox5.Checked then
    Include(tmpnf, nfAttributeChange);
  if CheckBox6.Checked then
    Include(tmpnf, nfSizeChange);
  if CheckBox7.Checked then
    Include(tmpnf, nfWriteChange);
  if CheckBox8.Checked then
    Include(tmpnf, nfAccessChange);
  if CheckBox9.Checked then
    Include(tmpnf, nfCreationDateChange);
  if CheckBox10.Checked then
    Include(tmpnf, nfSecurityChange);

  FileSystemWatcher1.NotifyFilters := tmpnf;
end;

end.

