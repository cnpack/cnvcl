unit UnitWatermark;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, jpeg,
  StdCtrls, ExtCtrls, CnNative, CnMatrix, CnDFT;

type
  TFormWatermark = class(TForm)
    btnOpenFile: TButton;
    dlgOpen1: TOpenDialog;
    img1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FJpg: TJPEGImage;
    FBmp: TBitmap;
  public

  end;

var
  FormWatermark: TFormWatermark;

implementation

uses
  CnDebug;

const
  WATERMARK_CELL_SIZE = 8;

{$R *.DFM}

procedure TFormWatermark.FormCreate(Sender: TObject);
begin
  FJpg := TJPEGImage.Create;
  FBmp := TBitmap.Create;
end;

procedure TFormWatermark.btnOpenFileClick(Sender: TObject);
var
  R, C, X, Y, W, H, B: Integer;
  M, T, MDCT, MIDCT: TCnFloatMatrix;
  P: PByte;
begin
  if dlgOpen1.Execute then
  begin
    FJpg.LoadFromFile(dlgOpen1.FileName);
    FBmp.Assign(FJpg);

    // CnDebugger.EvaluateObject(FBmp);
    case FBmp.PixelFormat of
      pf8bit:  B := 1;
      pf16bit: B := 2;
      pf24bit: B := 3;
      pf32bit: B := 4;
    else
      raise Exception.Create('PixelFormat Not Supported');
    end;

    W := FBmp.Width div WATERMARK_CELL_SIZE;
    H := FBmp.Height div WATERMARK_CELL_SIZE;

    M := TCnFloatMatrix.Create(WATERMARK_CELL_SIZE, WATERMARK_CELL_SIZE);
    T := TCnFloatMatrix.Create(WATERMARK_CELL_SIZE, WATERMARK_CELL_SIZE);

    MDCT := TCnFloatMatrix.Create;
    MIDCT := TCnFloatMatrix.Create;

    CnGenerateDCT2Matrix(MDCT, WATERMARK_CELL_SIZE);
    CnMatrixTranspose(MDCT, MIDCT);

    for R := 0 to H - 1 do
    begin
      for C := 0 to W - 1 do
      begin
        // 读第 [R, C] 个单元格到 M 中
        // 首行在 ScanLine[R * WATERMARK_CELL_SIZE]，首列在该 ScanLine 头 + C * WATERMARK_CELL_SIZE * BytesPerPixels

        for Y := 0 to WATERMARK_CELL_SIZE - 1 do
        begin
          P := FBmp.ScanLine[R * WATERMARK_CELL_SIZE + Y]; // P 指向本行开头
          P := Pointer(TCnNativeInt(P) + C * WATERMARK_CELL_SIZE * B); // P 指向本矩阵开头
          for X := 0 to WATERMARK_CELL_SIZE - 1 do
          begin
            // 此时 P 指向所需像素，读取 P 指的 1 2 3 4 个字节作为整数？
            M[X, Y] := P^; // 先只读一个字节
            // P^ := $00;
            Inc(P, B);
          end;
        end;

        // 读完了一个矩阵，做 DCT2
        CnDCT2(M, T, MDCT, MIDCT);

        // TODO: 给 T 处理水印
        T[0, 0] := 0;

        // 做逆 DCT2 还原
        CnIDCT2(T, M, MDCT, MIDCT);


        // 把 M 写回到第 [R, C] 个单元格中
        // 首行在 ScanLine[R * WATERMARK_CELL_SIZE]，首列在该 ScanLine 头 + C * WATERMARK_CELL_SIZE * BytesPerPixels

        for Y := 0 to WATERMARK_CELL_SIZE - 1 do
        begin
          P := FBmp.ScanLine[R * WATERMARK_CELL_SIZE + Y]; // P 指向本行开头
          P := Pointer(TCnNativeInt(P) + C * WATERMARK_CELL_SIZE * B); // P 指向本矩阵开头
          for X := 0 to WATERMARK_CELL_SIZE - 1 do
          begin
            // 此时 P 指向所需像素，
            P^ := Round(M[X, Y]);
            Inc(P, B);
          end;
        end;
      end;
    end;

    img1.Picture.Assign(FBmp);
    MIDCT.Free;
    MDCT.Free;
    T.Free;
    M.Free;
  end;
end;

procedure TFormWatermark.FormDestroy(Sender: TObject);
begin
  FBmp.Free;
  FJpg.Free;
end;

end.
