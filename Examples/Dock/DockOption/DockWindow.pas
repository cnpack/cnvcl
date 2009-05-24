unit DockWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnDockFormControl, ImgList, StdCtrls;

type
  TDockWindow_Form = class(TForm)
    CnDockClient1: TCnDockClient;
    Memo1: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DockWindow_Form: TDockWindow_Form;

implementation

uses MainForm;

{$R *.DFM}

end.
