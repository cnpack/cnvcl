program GB18030;

uses
  Forms,
  Unit18030 in 'Unit18030.pas' {FormGB18030},
  CnGB18030 in '..\..\Source\Common\CnGB18030.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormGB18030, FormGB18030);
  Application.Run;
end.
