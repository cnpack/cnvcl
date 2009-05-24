program AAFont;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := '平滑特效字体控件包演示程序';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
