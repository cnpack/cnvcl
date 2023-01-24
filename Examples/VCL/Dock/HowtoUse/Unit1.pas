unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnDockFormControl, CnVIDDockStyle, StdCtrls, CnClasses;

type
  TForm1 = class(TForm)
    CnDockServer1: TCnDockServer;
    CnVIDDockStyle1: TCnVIDDockStyle;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Unit2;

{$R *.DFM}

end.
