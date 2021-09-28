unit UnitGDIPlusTest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ImgList;

type
  TFormGDIPlus = class(TForm)
    btnStretchDraw: TButton;
    chkSmooth: TCheckBox;
    il1: TImageList;
    procedure btnStretchDrawClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormGDIPlus: TFormGDIPlus;

implementation

uses
  CnGraphUtils;

{$R *.DFM}

procedure TFormGDIPlus.btnStretchDrawClick(Sender: TObject);
var
  Src, Dst: TBitmap;
begin
  Src := TBitmap.Create;
  Src.PixelFormat := pf24bit;
  Src.Width := il1.Width;
  Src.Height := il1.Height;

  Dst := TBitmap.Create;
  Dst.PixelFormat := pf24bit;
  Dst.Width := 40;
  Dst.Height := 40;

  il1.GetBitmap(0, Src);
  Canvas.Draw(20, 80, Src);

  StretchDrawBmp(Src, Dst, True);
  Canvas.Draw(60, 80, Dst);

  Dst.Free;
  Src.Free;
end;

end.
