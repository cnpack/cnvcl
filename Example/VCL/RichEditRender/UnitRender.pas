unit UnitRender;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnRichEdit, StdCtrls, ExtCtrls, ComCtrls, CnNative;

type
  TFormRender = class(TForm)
    img1: TImage;
    btnTestRender: TButton;
    redt1: TRichEdit;
    btnTestLoad: TButton;
    btnTestSetText: TButton;
    dlgOpen1: TOpenDialog;
    Bevel1: TBevel;
    lblWidth: TLabel;
    edtWidth: TEdit;
    bvl1: TBevel;
    lblColor: TLabel;
    shp1: TShape;
    dlgColor1: TColorDialog;
    procedure btnTestRenderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnTestSetTextClick(Sender: TObject);
    procedure btnTestLoadClick(Sender: TObject);
    procedure shp1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FRender: TCnRichEditRender;
  public

  end;

var
  FormRender: TFormRender;

implementation

{$R *.DFM}

const
  RTF_CONTENT: AnsiString =
    '{\rtf1\ansi\ansicpg936\deff0\deflang1033\deflangfe2052{\fonttbl{\f0\fnil\fcharset134 \''cb\''ce\''cc\''e5;}{\f1\fnil\fcharset0 Arial Black;}{\f2\fnil\fcharset0 Microsoft Sans Serif;}{\f3\fnil\fcharset0 Courier New;}}' + #13#10 +
    '{\colortbl ;\red255\green0\blue0;\red0\green176\blue80;\red75\green172\blue198;}' + #13#10 +
    '{\*\generator Msftedit 5.41.21.2510;}\viewkind4\uc1\pard\sa200\sl276\slmult1\lang2052\b\f0\fs36 Hello\b0\fs22\par' + #13#10 +
    '\cf1\lang2052\f0 Red Text 啊红色文字，你是多么的美丽！Well, well come here to my house.\cf0\lang2052\f0\par' + #13#10 +
    '\cf2\lang1033\b\i\f2 Green Text\par' + #13#10 +
    '\cf3\b0\i0\strike\f3 Blue Text\par' + #13#10 +
    '\cf3\b0\i0\strike\f3 Blue Text\par' + #13#10 +
    '\cf3\b0\i0\strike\f3 Blue Text 222\par' + #13#10 +
    '\cf3\b0\i0\f2 Blue 333 Text\par' + #13#10 +
    '\cf3\b0\i0\strike\f3 Black Text\par' + #13#10 +
    '\cf0\lang2052\strike0\f0\par' + #13#10 +
    '}' + #13#10;

procedure TFormRender.btnTestRenderClick(Sender: TObject);
var
  Bmp: TBitmap;
  Mem: TMemoryStream;
  W: Integer;
begin
  redt1.Clear;
  W := StrToIntDef(edtWidth.Text, 0);
  FRender.BackgroundColor := shp1.Brush.Color;

  Mem := TMemoryStream.Create;
  Mem.WriteBuffer(RTF_CONTENT[1], Length(RTF_CONTENT));

  Mem.Position := 0;
  Bmp := FRender.RenderRtfToBitmap(Mem, W);
  if Bmp <> nil then
  begin
    img1.Picture.Assign(Bmp);
    Bmp.Free;
  end;
  Mem.Free;
end;

procedure TFormRender.FormCreate(Sender: TObject);
begin
  FRender := TCnRichEditRender.Create;
end;

procedure TFormRender.FormDestroy(Sender: TObject);
begin
  FRender.Free;
end;

procedure TFormRender.btnTestSetTextClick(Sender: TObject);
var
  Mem: TMemoryStream;
begin
  redt1.Clear;
  Mem := TMemoryStream.Create;
  Mem.WriteBuffer(RTF_CONTENT[1], Length(RTF_CONTENT));

  Mem.Position := 0;
  redt1.Lines.LoadFromStream(Mem);
  Mem.Free;
end;

procedure TFormRender.btnTestLoadClick(Sender: TObject);
begin
  if dlgOpen1.Execute then
    redt1.Lines.LoadFromFile(dlgOpen1.FileName);
end;

procedure TFormRender.shp1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if dlgColor1.Execute then
    shp1.Brush.Color := dlgColor1.Color;
end;

end.
