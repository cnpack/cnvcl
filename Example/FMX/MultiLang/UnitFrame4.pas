unit UnitFrame4;

interface

uses 
  {$IFDEF MSWINDOWS} Windows, Messages, {$ENDIF} SysUtils, Classes, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Types, System.Types, System.UITypes,
  FMX.Controls.Presentation;

type
  TFrame4 = class(TForm)
    grpFrame: TGroupBox;
    btn1: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
