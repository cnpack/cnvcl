program SunRiseSet;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  CnCalendar in '..\..\Source\Common\CnCalendar.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
