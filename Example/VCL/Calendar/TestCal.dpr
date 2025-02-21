program TestCal;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormCalendar, FormCalendar);
  Application.Run;
end.
