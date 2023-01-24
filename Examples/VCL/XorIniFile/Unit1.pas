unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ShellAPI;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    edtName: TEdit;
    lblName: TLabel;
    lblPasswd: TLabel;
    edtPasswd: TEdit;
    Bevel1: TBevel;
    lblXor: TLabel;
    edtSeed: TEdit;
    btnWrite: TButton;
    btnView: TButton;
    grp1: TGroupBox;
    btnRead: TButton;
    mmo1: TMemo;
    procedure btnWriteClick(Sender: TObject);
    procedure btnViewClick(Sender: TObject);
    procedure btnReadClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses CnIni;

{$R *.DFM}

const
  IniFileName = './Test.ini';

procedure TForm1.btnWriteClick(Sender: TObject);
var
  Ini: TCnXorIniFile;
begin
  Ini := TCnXorIniFile.Create(IniFileName, edtSeed.Text);
  try
    Ini.WriteString('Section', 'Name', edtName.Text);
    Ini.WriteString('Section', 'Password', edtPasswd.Text);

    if not FileExists(IniFileName) then
      Ini.SaveToFile(IniFileName);

    Application.MessageBox(PChar('File Write OK.'), 'Hint', MB_OK +
      MB_ICONINFORMATION);
  finally
    Ini.Free;
  end;
end;

procedure TForm1.btnViewClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', IniFileName, '', '', SW_SHOW);
end;

procedure TForm1.btnReadClick(Sender: TObject);
var
  Ini: TCnXorIniFile;
  Sections, Values: TStrings;
  I, J: Integer;
begin
  Ini := nil;
  Sections := nil;
  Values := nil;
  mmo1.Lines.Clear;

  try
    Ini := TCnXorIniFile.Create(IniFileName, edtSeed.Text);

    Sections := TStringList.Create;
    Values := TStringList.Create;

    Ini.ReadSections(Sections);
    for I := 0 to Sections.Count - 1 do
    begin
      mmo1.Lines.Add('[' + Sections[I] + ']');
      Ini.ReadSection(Sections[I], Values);
      for J := 0 to Values.Count - 1 do
        mmo1.Lines.Add(Values[J] + '=' + Ini.ReadString(Sections[I], Values[J], ''));
    end;

  finally
    Sections.Free;
    Ini.Free;
    Values.Free;
  end;
end;

end.
