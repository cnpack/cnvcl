unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Graphics, CnTwain;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    CnTwain1: TCnTwain;
    procedure Button1Click(Sender: TObject);
    procedure CnTwain1Captrue(Sender: TObject; bmp: TBitmap);
  private
    { Private declaCntions }
  public
    { Public declaCntions }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if not CnTwain1.IsDSOpen then
    CnTwain1.Acquire(True);
end;

procedure TForm1.CnTwain1Captrue(Sender: TObject; bmp: TBitmap);
begin
  Image1.Picture.Bitmap.Assign(bmp);
  if CnTwain1.IsDSOpen then
    CnTwain1.Terminate;
end;

end.
