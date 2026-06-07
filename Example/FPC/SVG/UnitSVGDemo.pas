unit UnitSVGDemo;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, CnCommon,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls;

type
  TFormSVGDemo = class(TForm)
    edtFileName: TEdit;
    btnBrowse: TButton;
    cmbSize: TComboBox;
    btnDraw: TButton;
    imgResult: TImage;
    dlgOpen: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnDrawClick(Sender: TObject);
  end;

var
  FormSVGDemo: TFormSVGDemo;

implementation

{$R *.lfm}

uses
  CnSVG;

procedure TFormSVGDemo.FormCreate(Sender: TObject);
begin
  cmbSize.ItemIndex := 3;
end;

procedure TFormSVGDemo.btnBrowseClick(Sender: TObject);
begin
  dlgOpen.Filter := 'SVG 文件 (*.svg)|*.svg';
  if dlgOpen.Execute then
    edtFileName.Text := dlgOpen.FileName;
end;

procedure TFormSVGDemo.btnDrawClick(Sender: TObject);
var
  SizeStr: string;
  SizeIndex: Integer;
  Size: Integer;
  Bitmap: TBitmap;
  R: TRect;
begin
  if edtFileName.Text = '' then
  begin
    MessageBox(Handle, '请先选择 SVG 文件。', '提示', MB_OK);
    Exit;
  end;

  if not FileExists(edtFileName.Text) then
  begin
    MessageBox(Handle, '文件不存在。', '错误', MB_OK);
    Exit;
  end;

  SizeIndex := cmbSize.ItemIndex;
  if SizeIndex < 0 then
    SizeIndex := 1; // 默认 128x128

  SizeStr := cmbSize.Items[SizeIndex];
  Size := StrToIntDef(Copy(SizeStr, 1, Pos('x', SizeStr) - 1), 128);

  // 创建临时位图，渲染 SVG 到位图，再显示到 TImage
  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := Size;
    Bitmap.Height := Size;
    Bitmap.PixelFormat := pf24bit;

    // 用白色背景填充
    Bitmap.Canvas.Brush.Color := clWhite;
    Bitmap.Canvas.FillRect(Rect(0, 0, Size, Size));

    R := Rect(0, 0, Size, Size);
    CnSVGRenderToCanvas(edtFileName.Text, Bitmap.Canvas, R);

    imgResult.Picture.Assign(Bitmap);
    imgResult.Width := Size;
    imgResult.Height := Size;
  finally
    Bitmap.Free;
  end;
end;

end.
