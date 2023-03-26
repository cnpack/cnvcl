unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ShellAPI, StdCtrls, CnOuterControls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    CnOuterControls1: TCnOuterControls;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  TxtHdl:THandle;
  EdtIdx:Integer;
begin
  ShellExecute(0,'open','notepad',nil,nil,SW_SHOW);
  Sleep(2000);
  CnOuterControls1.WindowCaption:='无标题 - 记事本';

  EdtIdx:=self.CnOuterControls1.ClassList.IndexOf('Edit');
  if EdtIdx = -1 then
  begin
    CnOuterControls1.WindowCaption:='Untitled - Notepad';
    EdtIdx:=self.CnOuterControls1.ClassList.IndexOf('Edit');
    if EdtIdx = -1 then
    begin
      MessageBox(Handle, 'Notepad not Found.', 'Error', MB_OK or MB_ICONERROR);
      Exit;
    end;  
  end;

  TxtHdl:=StrToInt(self.CnOuterControls1.HandleList.Strings[Edtidx]);
  CnOuterControls1.SndMsgHandle:=TxtHdl;
  CnOuterControls1.SndMessage:=WM_SETTEXT;
  CnOuterControls1.SndWParam:=0;
  CnOuterControls1.SndLParam:=Cardinal(PChar('这是外部程序控制控件写入的文本。'));
  CnOuterControls1.SendMessageToControl;
end;

end.
