{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2025 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��https://www.cnpack.org                                  }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnMandelbrotImage;
{* |<PRE>
================================================================================
* ������ƣ�����ؼ���
* ��Ԫ���ƣ����²��޼�ͼʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
* ��    ע�����㾫���� Extended ����Ӱ�첻�������ƷŴ�
*           �������������ر�����һ���������ʮ���ξ����ò��������ˣ�����ɾ��
*           �󸡵�������Ƚ�����һ����ָ�����ȵ�����һ�ٴεò�� 0.1 ��
* ����ƽ̨��PWin7 + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2020.07.03 V1.3
*               �����̣߳����ٽ���û�����ˡ�ɾ���ܲ���Ĵ�������ģʽ
*           2020.06.27 V1.2
*               �ô󸡵���ͬ��ʵ�����޷Ŵ󣬵������ٶ�Ҳ��
*           2019.12.21 V1.1
*               �ø߾�������������ʵ�����޷Ŵ󣬵������ٶȽ���
*           2019.12.18 V1.0
*               ������Ԫ��ʵ�ֹ��ܣ��� ScanLine ���ٻ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, Graphics, Controls, ExtCtrls, Contnrs,
  CnBigDecimal;

const
  CN_MANDELBROT_MAX_COUNT = 100;

type
  TCnMandelbrotMode = (mmFloat, mmBigDecimal, mmBigBinary);
  // ���������㡢�󸡵������㡢������Ƹ���������

  TCnMandelbrotFloatColorEvent = function (Sender: TObject; X, Y: Extended;
    XZ, YZ: Extended; Count: Integer): TColor of object;
  {* ������ģʽ�µ������ȡɫ�ʺ�����ע�� C ������� C > CN_MANDELBROT_MAX_COUNT ��ʾ������Ӧ�÷��������������ɫ}

  TCnMandelbrotDecimalColorEvent = function (Sender: TObject; X, Y: TCnBigDecimal;
    XZ, YZ: TCnBigDecimal; Count: Integer): TColor of object;
  {* �󸡵���ģʽ�µ������ȡɫ�ʺ�����ע�� C ������� C > CN_MANDELBROT_MAX_COUNT ��ʾ������Ӧ�÷��������������ɫ}

  TCnMandelbrotBinaryColorEvent = function (Sender: TObject; X, Y: TCnBigBinary;
    XZ, YZ: TCnBigBinary; Count: Integer): TColor of object;
  {* ������Ƹ�����ģʽ�µ������ȡɫ�ʺ�����ע�� C ������� C > CN_MANDELBROT_MAX_COUNT ��ʾ������Ӧ�÷��������������ɫ}

  TCnMandelbrotThreadCalcEvent = procedure (Sender: TObject; Progress, Total: Integer;
    var AbortCalc: Boolean) of object;
  {* �̼߳�����ڲ������¼�}

  TCnMandelbrotProgressEvent = procedure (Sender: TObject; Progress, Total: Integer) of object;
  {* �̼߳�����ڲ������¼�}

  TCnMandelbrotImage = class(TGraphicControl)
  {* ���²��޼�ͼʵ�ֿؼ�}
  private
    FLock: Boolean;   // �ؼ��ߴ�ı���Ե����ֵ�ı�ʱ�Ƿ��������¼���
    FThread: TThread;
    FBitmap: TBitmap;
    FXValues: array of Extended;
    FYValues: array of Extended;
    FXDecimals: TObjectList;
    FYDecimals: TObjectList;
    FXBinaries: TObjectList;
    FYBinaries: TObjectList;
    FMaxY: Extended;
    FMinX: Extended;
    FMinY: Extended;
    FMaxX: Extended;
    FMaxDX: TCnBigDecimal;
    FMinDX: TCnBigDecimal;
    FMaxDY: TCnBigDecimal;
    FMinDY: TCnBigDecimal;
    FMaxBX: TCnBigBinary;
    FMinBX: TCnBigBinary;
    FMaxBY: TCnBigBinary;
    FMinBY: TCnBigBinary;
    FOnFloatColor: TCnMandelbrotFloatColorEvent;
    FOnDecimalColor: TCnMandelbrotDecimalColorEvent;
    FOnBinaryColor: TCnMandelbrotBinaryColorEvent;
    FShowAxis: Boolean;
    FAxisColor: TColor;
    FMode: TCnMandelbrotMode;
    FInSetCount: Integer;
    FOutSetCount: Integer;
    FDigits: Integer;
    FOnThreadCalcEvent: TCnMandelbrotThreadCalcEvent;
    FOnProgress: TCnMandelbrotProgressEvent;                // ����������������㾫���õ�
    procedure UpdatePointsValues(AWidth, AHeight: Integer); // ��Եֵ�ı�ʱ���¸���ά�������ݸ�ֵ
    procedure UpdateMatrixes(AWidth, AHeight: Integer);     // �ߴ�ı�ʱ�������ɶ�ά����ֵ�������� UpdatePointsValues ���¸�ÿ��Ԫ�ظ�ֵ
    procedure SetMode(const Value: TCnMandelbrotMode);
    procedure SetShowAxis(const Value: Boolean);
    procedure SetAxisColor(const Value: TColor);
    procedure SetOnFloatColor(const Value: TCnMandelbrotFloatColorEvent);
    procedure SetOnDecimalColor(const Value: TCnMandelbrotDecimalColorEvent);
    procedure CheckLockedState;
    procedure SetOnBinaryColor(const Value: TCnMandelbrotBinaryColorEvent);
  protected
    // ���㵥�������ɫ
    function CalcFloatColor(X, Y: Extended; out InSet: Boolean): TColor;
    function CalcDecimalColor(X, Y: TCnBigDecimal; XZ, YZ: TCnBigDecimal; out InSet: Boolean): TColor;
    function CalcBinaryColor(X, Y: TCnBigBinary; XZ, YZ: TCnBigBinary; out InSet: Boolean): TColor;

    procedure TriggerCalcColors;
    procedure ReCalcColors;   // ���� FMode ��ֵ�ֱ�������������¼������е����ɫ���غ�ʱ��Ҫ���߳���ִ��

    procedure ReCalcFloatColors;
    procedure ReCalcBigDecimalColors;
    procedure ReCalcBigBinaryColors;
    procedure Paint; override;

    procedure DoProgress(Progress, Total: Integer); virtual;
    procedure ThreadTerminate(Sender: TObject);
    property OnThreadCalcEvent: TCnMandelbrotThreadCalcEvent read FOnThreadCalcEvent write FOnThreadCalcEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Loaded; override;
    
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    // ������������ģʽ�¸ı�ؼ����������ı�Ե��������ֵ���ؼ��ߴ粻��
    procedure SetRect(AMinX, AMaxX, AMinY, AMaxY: Extended); overload;
    procedure SetRect(AMinX, AMaxX, AMinY, AMaxY: TCnBigDecimal); overload;
    procedure SetRect(AMinX, AMaxX, AMinY, AMaxY: TCnBigBinary); overload;
    procedure AdjustDigits;

    procedure GetComplexValues(X, Y: Integer; out R, I: Extended);
    procedure GetComplexDecimal(X, Y: Integer; R, I: TCnBigDecimal);
    procedure GetComplexBinary(X, Y: Integer; R, I: TCnBigBinary);
    function GetCurrentCalcDigits: Integer;
    {* ���ص�ǰ�ļ��㾫�ȣ�Ҳ����С�����ڼ�λ}
    procedure Lock;
    procedure UnLock;
  published
    property Mode: TCnMandelbrotMode read FMode write SetMode;
    {* ����ģʽ����ʹ�þ������޵���չ���ȸ��㣬���Ǵ󸡵���}

    property MinX: Extended read FMinX;
    {* ������ģʽ�µ� X �����ֵ}
    property MinY: Extended read FMinY;
    {* ������ģʽ�µ� Y ����Եֵ}
    property MaxX: Extended read FMaxX;
    {* ������ģʽ�µ� X �����ֵ}
    property MaxY: Extended read FMaxY;
    {* ������ģʽ�µ� Y ����Եֵ}

    property MinDX: TCnBigDecimal read FMinDX;
    {* �󸡵���ģʽ�µ� X �����ֵ}
    property MinDY: TCnBigDecimal read FMinDY;
    {* �󸡵���ģʽ�µ� Y ����Եֵ}
    property MaxDX: TCnBigDecimal read FMaxDX;
    {* �󸡵���ģʽ�µ� X �����ֵ}
    property MaxDY: TCnBigDecimal read FMaxDY;
    {* �󸡵���ģʽ�µ� Y ����Եֵ}

    property MinBX: TCnBigBinary read FMinBX;
    {* ������Ƹ�����ģʽ�µ� X �����ֵ}
    property MinBY: TCnBigBinary read FMinBY;
    {* ������Ƹ�����ģʽ�µ� Y ����Եֵ}
    property MaxBX: TCnBigBinary read FMaxBX;
    {* ������Ƹ�����ģʽ�µ� X �����ֵ}
    property MaxBY: TCnBigBinary read FMaxBY;
    {* ������Ƹ�����ģʽ�µ� Y ����Եֵ}

    property InSetCount: Integer read FInSetCount;
    {* һ�����������У������ڼ����ڵĵ�����}
    property OutSetCount: Integer read FOutSetCount;
    {* һ�����������У������ڼ�����ĵ�����}

    property OnColor: TCnMandelbrotFloatColorEvent read FOnFloatColor write SetOnFloatColor;
    {* �Զ��帡��ģʽ�����²��޼����ص����ɫ�¼������ޣ����ڲ�ʹ�ô�ɫ����}
    property OnDecimalColor: TCnMandelbrotDecimalColorEvent read FOnDecimalColor
      write SetOnDecimalColor;
    {* �Զ���󸡵���ģʽ�����²��޼����ص����ɫ�¼������ޣ����ڲ�ʹ�ô�ɫ����}
    property OnBinaryColor: TCnMandelbrotBinaryColorEvent read FOnBinaryColor
      write SetOnBinaryColor;
    {* �Զ���󸡵���ģʽ�����²��޼����ص����ɫ�¼������ޣ����ڲ�ʹ�ô�ɫ����}

    property OnProgress: TCnMandelbrotProgressEvent read FOnProgress write FOnProgress;
    {* ��������¼�}

    property ShowAxis: Boolean read FShowAxis write SetShowAxis;
    {* �Ƿ����������}
    property AxisColor: TColor read FAxisColor write SetAxisColor;
    {* ��������ɫ}
    property OnClick;
    {* ����¼����}
  end;

implementation

resourcestring
  SCnMandelbrotOutOfBounds = 'Invalid Mode or X Y Out of Bounds.';

type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [Byte] of TRGBTriple;

  TCnMandelbrotThread = class(TThread)
  private
    FProgress: Integer;
    FTotal: Integer;
    FImage: TCnMandelbrotImage;
    procedure OnImageCalcEvent(Sender: TObject; Progress, Total: Integer;
      var AbortCalc: Boolean);
    procedure NotifyProgress;
  protected
    procedure Execute; override;
  public
    property Image: TCnMandelbrotImage read FImage write FImage;
  end;

var
  TmpDXZ: TCnBigDecimal = nil;
  TmpDYZ: TCnBigDecimal = nil;
  TmpBXZ: TCnBigBinary = nil;
  TmpBYZ: TCnBigBinary = nil;

  Decimal4: TCnBigDecimal = nil;
  Binary4: TCnBigBinary = nil;

procedure CalcMandelbortSetFloatPoint(X, Y: Extended; out XZ, YZ: Extended; out Count: Integer);
var
  XZ2, YZ2: Extended;
begin
  XZ := 0.0;
  YZ := 0.0;
  Count := 0;

  if X * X + Y * Y > 4.0 then
    Exit;

  repeat
    // XZ + YZi := (XZ + YZi)^2 + (X + Yi);
    XZ2 := XZ * XZ;
    YZ2 := YZ * YZ;

    // ���ε�����������Ҫ���� XZ^2 �� YZ^2 ��ֵ��������;�����ı�
    YZ := 2.0 * XZ * YZ + Y;
    XZ := XZ2 - YZ2 + X;
    Inc(Count);
  until (XZ * XZ + YZ * YZ > 4.0) or (Count > CN_MANDELBROT_MAX_COUNT);
end;

procedure CalcMandelbortSetDecimalPoint(X, Y: TCnBigDecimal; XZ, YZ: TCnBigDecimal;
  const Digits: Integer; out Count: Integer);

  function D2SqrSumGT4(A, B: TCnBigDecimal): Boolean;
  begin
    Result := False;
    BigDecimalCopy(TmpDXZ, A);
    BigDecimalCopy(TmpDYZ, B);
    BigDecimalMul(TmpDXZ, TmpDXZ, TmpDXZ, Digits);
    BigDecimalMul(TmpDYZ, TmpDYZ, TmpDYZ, Digits);
    BigDecimalAdd(TmpDXZ, TmpDXZ, TmpDYZ);

    if BigDecimalCompare(TmpDXZ, Decimal4) > 0 then
      Result := True;
  end;

begin
  // �Դ󸡵����ķ�ʽ��������
  if TmpDXZ = nil then
    TmpDXZ := TCnBigDecimal.Create;
  if TmpDYZ = nil then
    TmpDYZ := TCnBigDecimal.Create;

  Count := 0;
  if D2SqrSumGT4(X, Y) then
    Exit;

  repeat
    BigDecimalCopy(TmpDXZ, XZ);
    BigDecimalCopy(TmpDYZ, YZ);
    BigDecimalMul(TmpDXZ, TmpDXZ, XZ, Digits);
    BigDecimalMul(TmpDYZ, TmpDYZ, YZ, Digits);

    BigDecimalMul(YZ, YZ, XZ, Digits);
    YZ.MulWord(2);
    BigDecimalAdd(YZ, YZ, Y);

    BigDecimalCopy(XZ, TmpDXZ);
    BigDecimalSub(XZ, XZ, TmpDYZ);
    BigDecimalAdd(XZ, XZ, X);

    Inc(Count);
  until D2SqrSumGT4(XZ, YZ) or (Count > CN_MANDELBROT_MAX_COUNT);
end;

procedure CalcMandelbortSetBinaryPoint(X, Y: TCnBigBinary; XZ, YZ: TCnBigBinary;
  const Digits: Integer; out Count: Integer);

  function D2SqrSumGT4(A, B: TCnBigBinary): Boolean;
  begin
    Result := False;
    BigBinaryCopy(TmpBXZ, A);
    BigBinaryCopy(TmpBYZ, B);
    BigBinaryMul(TmpBXZ, TmpBXZ, TmpBXZ, Digits);
    BigBinaryMul(TmpBYZ, TmpBYZ, TmpBYZ, Digits);
    BigBinaryAdd(TmpBXZ, TmpBXZ, TmpBYZ);

    if BigBinaryCompare(TmpBXZ, Binary4) > 0 then
      Result := True;
  end;

begin
  // �Դ�����Ƹ������ķ�ʽ��������
  if TmpBXZ = nil then
    TmpBXZ := TCnBigBinary.Create;
  if TmpBYZ = nil then
    TmpBYZ := TCnBigBinary.Create;

  Count := 0;
  if D2SqrSumGT4(X, Y) then
    Exit;

  repeat
    BigBinaryCopy(TmpBXZ, XZ);
    BigBinaryCopy(TmpBYZ, YZ);
    BigBinaryMul(TmpBXZ, TmpBXZ, XZ, Digits);
    BigBinaryMul(TmpBYZ, TmpBYZ, YZ, Digits);

    BigBinaryMul(YZ, YZ, XZ, Digits);
    YZ.MulWord(2);
    BigBinaryAdd(YZ, YZ, Y);

    BigBinaryCopy(XZ, TmpBXZ);
    BigBinarySub(XZ, XZ, TmpBYZ);
    BigBinaryAdd(XZ, XZ, X);

    Inc(Count);
  until D2SqrSumGT4(XZ, YZ) or (Count > CN_MANDELBROT_MAX_COUNT);
end;

{ TCnMandelbrotImage }

function TCnMandelbrotImage.CalcFloatColor(X, Y: Extended; out InSet: Boolean): TColor;
var
  XZ, YZ: Extended;
  C: Integer;
begin
  XZ := 0.0;
  YZ := 0.0;
  C := 0;

  CalcMandelbortSetFloatPoint(X, Y, XZ, YZ, C);

  if C > CN_MANDELBROT_MAX_COUNT then
  begin
    InSet := True;
    if Assigned(FOnFloatColor) then
      Result := FOnFloatColor(Self, X, Y, XZ, YZ, C)
    else
      Result := clNavy;
  end
  else
  begin
    InSet := False;
    if Assigned(FOnFloatColor) then
      Result := FOnFloatColor(Self, X, Y, XZ, YZ, C)
    else
      Result := clWhite;
  end;
end;

function TCnMandelbrotImage.CalcDecimalColor(X, Y, XZ,
  YZ: TCnBigDecimal; out InSet: Boolean): TColor;
var
  C: Integer;
begin
  XZ.SetZero;
  YZ.SetZero;
  C := 0;

  CalcMandelbortSetDecimalPoint(X, Y, XZ, YZ, FDigits, C);

  if C > CN_MANDELBROT_MAX_COUNT then
  begin
    InSet := True;
    if Assigned(FOnDecimalColor) then
      Result := FOnDecimalColor(Self, X, Y, XZ, YZ, C)
    else
      Result := clNavy;
  end
  else
  begin
    InSet := False;
    if Assigned(FOnDecimalColor) then
      Result := FOnDecimalColor(Self, X, Y, XZ, YZ, C)
    else
      Result := clWhite;
  end;
end;

function TCnMandelbrotImage.CalcBinaryColor(X, Y, XZ,
  YZ: TCnBigBinary; out InSet: Boolean): TColor;
var
  C: Integer;
begin
  XZ.SetZero;
  YZ.SetZero;
  C := 0;

  CalcMandelbortSetBinaryPoint(X, Y, XZ, YZ, FDigits, C);

  if C > CN_MANDELBROT_MAX_COUNT then
  begin
    InSet := True;
    if Assigned(FOnBinaryColor) then
      Result := FOnBinaryColor(Self, X, Y, XZ, YZ, C)
    else
      Result := clNavy;
  end
  else
  begin
    InSet := False;
    if Assigned(FOnBinaryColor) then
      Result := FOnBinaryColor(Self, X, Y, XZ, YZ, C)
    else
      Result := clWhite;
  end;
end;

constructor TCnMandelbrotImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMinX := -2.0;
  FMaxX := 1.0;
  FMinY := -1.5;
  FMaxY := 1.5;

  FDigits := 8; // ʮ���Ƶ�Ĭ�Ͼ���

  FAxisColor := clTeal;
  FXDecimals := TObjectList.Create(True);
  FYDecimals := TObjectList.Create(True);
  FXBinaries := TObjectList.Create(True);
  FYBinaries := TObjectList.Create(True);

  FMaxDX := TCnBigDecimal.Create;
  FMinDX := TCnBigDecimal.Create;
  FMaxDY := TCnBigDecimal.Create;
  FMinDY := TCnBigDecimal.Create;

  FMaxBX := TCnBigBinary.Create;
  FMinBX := TCnBigBinary.Create;
  FMaxBY := TCnBigBinary.Create;
  FMinBY := TCnBigBinary.Create;
end;

destructor TCnMandelbrotImage.Destroy;
begin
  if FThread <> nil then
  begin
    FThread.Terminate;
    try
      FThread.WaitFor;
    except
      ;
    end;
    FThread := nil;
  end;

  FMaxBX.Free;
  FMinBX.Free;
  FMaxBY.Free;
  FMinBY.Free;

  FMaxDX.Free;
  FMinDX.Free;
  FMaxDY.Free;
  FMinDY.Free;

  FXBinaries.Free;
  FYBinaries.Free;
  FYDecimals.Free;
  FXDecimals.Free;

  FBitmap.Free;
  SetLength(FXValues, 0);
  SetLength(FYValues, 0);
  inherited;
end;

procedure TCnMandelbrotImage.GetComplexValues(X, Y: Integer; out R,
  I: Extended);
begin
  if (FMode = mmFloat) and (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height) then
  begin
    R := FXValues[X];
    I := FYValues[Y];
  end
  else
    raise Exception.Create(SCnMandelbrotOutOfBounds);
end;

procedure TCnMandelbrotImage.GetComplexDecimal(X, Y: Integer; R,
  I: TCnBigDecimal);
begin
  if (FMode = mmBigDecimal) and (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height) then
  begin
    BigDecimalCopy(R, TCnBigDecimal(FXDecimals[X]));
    BigDecimalCopy(I, TCnBigDecimal(FYDecimals[Y]));
  end
  else
    raise Exception.Create(SCnMandelbrotOutOfBounds);
end;

procedure TCnMandelbrotImage.GetComplexBinary(X, Y: Integer; R,
  I: TCnBigBinary);
begin
  if (FMode = mmBigBinary) and (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height) then
  begin
    BigBinaryCopy(R, TCnBigBinary(FXBinaries[X]));
    BigBinaryCopy(I, TCnBigBinary(FYBinaries[Y]));
  end
  else
    raise Exception.Create(SCnMandelbrotOutOfBounds);
end;

procedure TCnMandelbrotImage.Loaded;
begin
  inherited;
  CheckLockedState;
end;

procedure TCnMandelbrotImage.Paint;
var
  X, Y: Integer;
  DD, DT: TCnBigDecimal;
  BD, BT: TCnBigBinary;
begin
  Canvas.Draw(0, 0, FBitmap);

  if ShowAxis then
  begin
    // ��� X Y ���λ�ã�����
    if FMode = mmFloat then
    begin
      X := Trunc(Width * (-FMinX) / (FMaxX - FMinX));
      Y := Trunc(Height * (FMaxY) / (FMaxY - FMinY));
    end
    else if FMode = mmBigDecimal then
    begin
      DD := TCnBigDecimal.Create;
      DT := TCnBigDecimal.Create;
      try
        BigDecimalSub(DD, FMaxDX, FMinDX);
        BigDecimalCopy(DT, FMinDX);
        DT.Negate;
        DT.MulWord(Width);
        BigDecimalDiv(DT, DT, DD);
        X := Trunc(BigDecimalToExtended(DT));

        BigDecimalSub(DD, FMaxDY, FMinDY);
        BigDecimalCopy(DT, FMaxDY);
        DT.MulWord(Height);
        BigDecimalDiv(DT, DT, DD);
        Y := Trunc(BigDecimalToExtended(DT));
      finally
        DT.Free;
        DD.Free;
      end;
    end
    else if FMode = mmBigBinary then
    begin
      BD := TCnBigBinary.Create;
      BT := TCnBigBinary.Create;
      try
        BigBinarySub(BD, FMaxBX, FMinBX);
        BigBinaryCopy(BT, FMinBX);
        BT.Negate;
        BT.MulWord(Width);
        BigBinaryDiv(BT, BT, BD);
        X := Trunc(BigBinaryToExtended(BT));

        BigBinarySub(BD, FMaxBY, FMinBY);
        BigBinaryCopy(BT, FMaxBY);
        BT.MulWord(Height);
        BigBinaryDiv(BT, BT, BD);
        Y := Trunc(BigBinaryToExtended(BT));
      finally
        BT.Free;
        BD.Free;
      end;
    end;

    Canvas.Pen.Color := FAxisColor;
    Canvas.Pen.Style := psSolid;
    Canvas.MoveTo(X, 0);
    Canvas.LineTo(X, Height);
    Canvas.MoveTo(0, Y);
    Canvas.LineTo(Width, Y);
  end;
end;

procedure TCnMandelbrotImage.ReCalcFloatColors;
var
  X, Y, C: Integer;
  AColor: TColor;
  R, G, B: Byte;
  Arr: PRGBTripleArray;
  InSet: Boolean;
begin
  FInSetCount := 0;
  FOutSetCount := 0;

  for Y := 0 to Height - 1 do
  begin
    Arr := PRGBTripleArray(FBitmap.ScanLine[Y]);
    for X := 0 to Width - 1 do
    begin
      AColor := CalcFloatColor(FXValues[X], FYValues[Y], InSet);
      if InSet then
        Inc(FInSetCount)
      else
        Inc(FOutSetCount);

      C := ColorToRGB(AColor);
      B := C and $FF0000 shr 16;
      G := C and $00FF00 shr 8;
      R := C and $0000FF;

      Arr^[X].rgbtRed := R;
      Arr^[X].rgbtGreen := G;
      Arr^[X].rgbtBlue := B;
    end;
  end;
  Invalidate;
end;

procedure TCnMandelbrotImage.ReCalcBigDecimalColors;
var
  X, Y, C: Integer;
  AColor: TColor;
  R, G, B: Byte;
  Arr: PRGBTripleArray;
  XZ, YZ: TCnBigDecimal;
  InSet, AbortCalc: Boolean;
begin
  FInSetCount := 0;
  FOutSetCount := 0;

  XZ := nil;
  YZ := nil;
  AbortCalc := False;

  try
    XZ := TCnBigDecimal.Create;
    YZ := TCnBigDecimal.Create;

    for Y := 0 to Height - 1 do
    begin
      Arr := PRGBTripleArray(FBitmap.ScanLine[Y]);
      for X := 0 to Width - 1 do
      begin
        AColor := CalcDecimalColor(TCnBigDecimal(FXDecimals[X]),
          TCnBigDecimal(FYDecimals[Y]), XZ, YZ, InSet);

        if InSet then
          Inc(FInSetCount)
        else
          Inc(FOutSetCount);

        C := ColorToRGB(AColor);
        B := C and $FF0000 shr 16;
        G := C and $00FF00 shr 8;
        R := C and $0000FF;

        Arr^[X].rgbtRed := R;
        Arr^[X].rgbtGreen := G;
        Arr^[X].rgbtBlue := B;
      end;

      if Assigned(FOnThreadCalcEvent) then
        FOnThreadCalcEvent(Self, Y, Height - 1, AbortCalc);

      if AbortCalc then
        Exit;
    end;
  finally
    XZ.Free;
    YZ.Free;
  end;
  // Invalidate;
end;

procedure TCnMandelbrotImage.ReCalcBigBinaryColors;
var
  X, Y, C: Integer;
  AColor: TColor;
  R, G, B: Byte;
  Arr: PRGBTripleArray;
  XZ, YZ: TCnBigBinary;
  InSet, AbortCalc: Boolean;
begin
  FInSetCount := 0;
  FOutSetCount := 0;

  XZ := nil;
  YZ := nil;
  AbortCalc := False;

  try
    XZ := TCnBigBinary.Create;
    YZ := TCnBigBinary.Create;

    for Y := 0 to Height - 1 do
    begin
      Arr := PRGBTripleArray(FBitmap.ScanLine[Y]);
      for X := 0 to Width - 1 do
      begin
        AColor := CalcBinaryColor(TCnBigBinary(FXBinaries[X]),
          TCnBigBinary(FYBinaries[Y]), XZ, YZ, InSet);

        if InSet then
          Inc(FInSetCount)
        else
          Inc(FOutSetCount);

        C := ColorToRGB(AColor);
        B := C and $FF0000 shr 16;
        G := C and $00FF00 shr 8;
        R := C and $0000FF;

        Arr^[X].rgbtRed := R;
        Arr^[X].rgbtGreen := G;
        Arr^[X].rgbtBlue := B;
      end;

      if Assigned(FOnThreadCalcEvent) then
        FOnThreadCalcEvent(Self, Y, Height - 1, AbortCalc);

      if AbortCalc then
        Exit;
    end;
  finally
    XZ.Free;
    YZ.Free;
  end;
  // Invalidate;
end;

procedure TCnMandelbrotImage.SetAxisColor(const Value: TColor);
begin
  if Value <> FAxisColor then
  begin
    FAxisColor := Value;
    Invalidate;
  end;
end;

procedure TCnMandelbrotImage.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  CheckLockedState;
end;

procedure TCnMandelbrotImage.SetMode(const Value: TCnMandelbrotMode);
begin
  if Value <> FMode then
  begin
    FMode := Value;

    FMinDX.SetExtended(FMinX);
    FMinDY.SetExtended(FMinY);
    FMaxDX.SetExtended(FMaxX);
    FMaxDY.SetExtended(FMaxY);

    FMinBX.SetExtended(FMinX);
    FMinBY.SetExtended(FMinY);
    FMaxBX.SetExtended(FMaxX);
    FMaxBY.SetExtended(FMaxY);

    AdjustDigits;
    CheckLockedState;
  end;
end;

procedure TCnMandelbrotImage.SetRect(AMinX, AMaxX, AMinY, AMaxY: Extended);
begin
  if FMode = mmFloat then
  begin
    FMinX := AMinX;
    FMinY := AMinY;
    FMaxX := AMaxX;
    FMaxY := AMaxY;

    CheckLockedState;
  end;
end;

procedure TCnMandelbrotImage.SetRect(AMinX, AMaxX, AMinY,
  AMaxY: TCnBigDecimal);
begin
  if FMode = mmBigDecimal then
  begin
    BigDecimalCopy(FMinDX, AMinX);
    BigDecimalCopy(FMinDY, AMinY);
    BigDecimalCopy(FMaxDX, AMaxX);
    BigDecimalCopy(FMaxDY, AMaxY);

    AdjustDigits;
    CheckLockedState;
  end;
end;

procedure TCnMandelbrotImage.SetRect(AMinX, AMaxX, AMinY,
  AMaxY: TCnBigBinary);
begin
  if FMode = mmBigBinary then
  begin
    BigBinaryCopy(FMinBX, AMinX);
    BigBinaryCopy(FMinBY, AMinY);
    BigBinaryCopy(FMaxBX, AMaxX);
    BigBinaryCopy(FMaxBY, AMaxY);

    AdjustDigits;
    CheckLockedState;
  end;
end;

procedure TCnMandelbrotImage.SetShowAxis(const Value: Boolean);
begin
  if Value <> FShowAxis then
  begin
    FShowAxis := Value;
    Invalidate;
  end;
end;

procedure TCnMandelbrotImage.UpdateMatrixes(AWidth, AHeight: Integer);
var
  I: Integer;
begin
  if FMode = mmFloat then
  begin
    // �жϲ����³�ʼ�� X��Y �ĸ���������
    if Length(FXValues) <> AWidth then
      SetLength(FXValues, AWidth);
    if Length(FYValues) <> AHeight then
      SetLength(FYValues, AHeight);
  end
  else if FMode = mmBigDecimal then
  begin
    // �жϲ����³�ʼ�� X��Y �Ĵ󸡵����б�
    if FXDecimals.Count <> AWidth then
    begin
      FXDecimals.Clear;
      for I := 1 to AWidth do
        FXDecimals.Add(TCnBigDecimal.Create);
    end;
    if FYDecimals.Count <> AHeight then
    begin
      FYDecimals.Clear;
      for I := 1 to AHeight do
        FYDecimals.Add(TCnBigDecimal.Create);
    end;
  end
  else if FMode = mmBigBinary then
  begin
    // �жϲ����³�ʼ�� X��Y �Ĵ�����Ƹ������б�
    if FXBinaries.Count <> AWidth then
    begin
      FXBinaries.Clear;
      for I := 1 to AWidth do
        FXBinaries.Add(TCnBigBinary.Create);
    end;
    if FYBinaries.Count <> AHeight then
    begin
      FYBinaries.Clear;
      for I := 1 to AHeight do
        FYBinaries.Add(TCnBigBinary.Create);
    end;
  end;

  // �жϲ����³�ʼ���ڲ�λͼ
  if (FBitmap = nil) or ((FBitmap.Width <> AWidth) or (FBitmap.Height <> AHeight)) then
  begin
    FreeAndNil(FBitmap);
    FBitmap := TBitmap.Create;
    FBitmap.PixelFormat := pf24bit;
    FBitmap.Width := AWidth;
    FBitmap.Height := AHeight;
  end;

  UpdatePointsValues(AWidth, AHeight);
end;

procedure TCnMandelbrotImage.UpdatePointsValues(AWidth, AHeight: Integer);
var
  X, Y, W, H: Integer;
  WX, HY: Extended;
  WDX, HDY: TCnBigDecimal;
  WBX, HBY: TCnBigBinary;
begin
  W := Width - 1;
  H := Height - 1;
  if FMode = mmFloat then
  begin
    WX := (FMaxX - FMinX) / W;
    HY := (FMaxY - FMinY) / H;

    for X := 0 to W do
      FXValues[X] := FMinX + X * WX;

    for Y := 0 to H do
      FYValues[Y] := FMinY + (H - Y) * HY;
  end
  else if FMode = mmBigDecimal then
  begin
    // ��ʼ�� X��Y �Ĵ󸡵�����ֵ
    WDX := TCnBigDecimal.Create;
    HDY := TCnBigDecimal.Create;

    BigDecimalSub(WDX, FMaxDX, FMinDX);
    WDX.DivWord(W);
    BigDecimalSub(HDY, FMaxDY, FMinDY);
    HDY.DivWord(H);

    for X := 0 to W do
    begin
      BigDecimalCopy(TCnBigDecimal(FXDecimals[X]), WDX);
      TCnBigDecimal(FXDecimals[X]).MulWord(X);
      BigDecimalAdd(TCnBigDecimal(FXDecimals[X]), FMinDX, TCnBigDecimal(FXDecimals[X]));
    end;

    for Y := 0 to H do
    begin
      BigDecimalCopy(TCnBigDecimal(FYDecimals[Y]), HDY);
      TCnBigDecimal(FYDecimals[Y]).MulWord(H - Y);
      BigDecimalAdd(TCnBigDecimal(FYDecimals[Y]), FMinDY, TCnBigDecimal(FYDecimals[Y]));
    end;
  end
  else if FMode = mmBigBinary then
  begin
    // ��ʼ�� X��Y �Ĵ�����Ƹ�������ֵ
    WBX := TCnBigBinary.Create;
    HBY := TCnBigBinary.Create;

    BigBinarySub(WBX, FMaxBX, FMinBX);
    WBX.DivWord(W);
    BigBinarySub(HBY, FMaxBY, FMinBY);
    HBY.DivWord(H);

    for X := 0 to W do
    begin
      BigBinaryCopy(TCnBigBinary(FXBinaries[X]), WBX);
      TCnBigBinary(FXBinaries[X]).MulWord(X);
      BigBinaryAdd(TCnBigBinary(FXBinaries[X]), FMinBX, TCnBigBinary(FXBinaries[X]));
    end;

    for Y := 0 to H do
    begin
      BigBinaryCopy(TCnBigBinary(FYBinaries[Y]), HBY);
      TCnBigBinary(FYBinaries[Y]).MulWord(H - Y);
      BigBinaryAdd(TCnBigBinary(FYBinaries[Y]), FMinBY, TCnBigBinary(FYBinaries[Y]));
    end;
  end;

  FBitmap.Canvas.Brush.Color := clWhite;
  FBitmap.Canvas.Brush.Style := bsSolid;
  FBitmap.Canvas.FillRect(Rect(0, 0, AHeight, AWidth));
end;

procedure TCnMandelbrotImage.ReCalcColors;
begin
  if FMode = mmFloat then
    ReCalcFloatColors
  else if FMode = mmBigDecimal then
    RecalcBigDecimalColors
  else if FMode = mmBigBinary then
    ReCalcBigBinaryColors;
end;

procedure TCnMandelbrotImage.Lock;
begin
  FLock := True;
end;

procedure TCnMandelbrotImage.UnLock;
begin
  FLock := False;
  CheckLockedState;
end;

procedure TCnMandelbrotImage.CheckLockedState;
begin
  if not (csLoading in ComponentState) and not FLock then
  begin
    UpdateMatrixes(Width, Height); // �м����� UpdatePointsValue
    TriggerCalcColors;
  end;
end;

procedure TCnMandelbrotImage.SetOnDecimalColor(
  const Value: TCnMandelbrotDecimalColorEvent);
begin
  FOnDecimalColor := Value;
  CheckLockedState;
end;

procedure TCnMandelbrotImage.TriggerCalcColors;
begin
  if FThread <> nil then
  begin
    FThread.Terminate;
    try
      FThread.WaitFor;
    except
      ;
    end;
    FThread := nil;
  end;

  if Mode = mmFloat then
  begin
    ReCalcColors;
    Exit;
  end;

  if FThread = nil then
  begin
    FThread := TCnMandelbrotThread.Create(True);
    TCnMandelbrotThread(FThread).Image := Self;
    FOnThreadCalcEvent := TCnMandelbrotThread(FThread).OnImageCalcEvent;
    FThread.FreeOnTerminate := True;
    FThread.OnTerminate := ThreadTerminate;

    FThread.Resume;
  end;
end;

procedure TCnMandelbrotImage.DoProgress(Progress, Total: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Progress, Total);
  Invalidate;
end;

procedure TCnMandelbrotImage.ThreadTerminate(Sender: TObject);
begin
  FThread := nil;
  Invalidate;
end;

procedure TCnMandelbrotImage.SetOnFloatColor(
  const Value: TCnMandelbrotFloatColorEvent);
begin
  FOnFloatColor := Value;
  CheckLockedState;
end;

function TCnMandelbrotImage.GetCurrentCalcDigits: Integer;
begin
  Result := FDigits;
end;

procedure TCnMandelbrotImage.AdjustDigits;
var
  DW, DH: TCnBigDecimal;
  BW, BH: TCnBigBinary;
  N: Integer;
begin
  // ���ݿؼ��ߴ���λ��ȷ�����㾫�ȣ���֪ 800 ���أ�3 �ľ��룬���㾫��ʮ���� 8�������� 32 ����Ҫ��

  if FMode = mmBigDecimal then
  begin
    // �Ʋ⣺�����������ص�ֵ�Ĳ���������㾫�ȣ�0.00375 ��Ҫ���� 8����ָ��������0.0375 ֻ��Ҫ 7��0.375 ֻ��Ҫ 6
    // �򻯳� 0.001 ��Ҫ 8��10^-3 ��Ҫ 8��10^-2 ��Ҫ 7��10^-n ��Ҫ 5 + n
    // ���� n �����λ��Ч������С��������λ

    DW := TCnBigDecimal.Create;
    DH := TCnBigDecimal.Create;
    try
      BigDecimalSub(DW, FMaxDX, FMinDX);
      BigDecimalSub(DH, FMaxDY, FMinDY);

      DW.DivWord(Width);
      DH.DivWord(Height);

      if DW.IsNegative then
        DW.Negate;
      if DH.IsNegative then
        DH.Negate;

      if BigDecimalCompare(DW, DH) >= 0 then
      begin
        // DH ��С
        N := BigDecimalGetHighScale(DH);
      end
      else
      begin
        // DW ��С
        N := BigDecimalGetHighScale(DW);
      end;

      if N > 0 then
      begin
        N := N + 5;
        FDigits := N;
      end;
    finally
      DW.Free;
      DH.Free;
    end;
  end
  else if FMode = mmBigBinary then
  begin
    // �Ʋ⣺�����������ص�ֵ�Ĳ���������㾫�ȣ�0.00375 ��Ҫ�����ƾ��� 32
    // �򻯳� 2 �� -8 �η���Ҫ 32 λ���ȣ���ô 2 �� -9 �η�����Ҫ 33 λ���ȣ�2 �� -n �η�����Ҫ 24 + n λ����
    // ���� n �����λ��Ч�����Ƕ�����С��������λ

    BW := TCnBigBinary.Create;
    BH := TCnBigBinary.Create;
    try
      BigBinarySub(BW, FMaxBX, FMinBX);
      BigBinarySub(BH, FMaxBY, FMinBY);

      BW.DivWord(Width);
      BH.DivWord(Height);

      if BW.IsNegative then
        BW.Negate;
      if BH.IsNegative then
        BH.Negate;

      if BigBinaryCompare(BW, BH) >= 0 then
      begin
        // BH ��С
        N := BigBinaryGetHighScale(BH);
      end
      else
      begin
        // BW ��С
        N := BigBinaryGetHighScale(BW);
      end;

      if N > 0 then
      begin
        N := N + 24;
        FDigits := N;
      end;
    finally
      BW.Free;
      BH.Free;
    end;
  end;
end;

procedure TCnMandelbrotImage.SetOnBinaryColor(
  const Value: TCnMandelbrotBinaryColorEvent);
begin
  FOnBinaryColor := Value;
  CheckLockedState;
end;

{ TCnMandelbrotThread }

procedure TCnMandelbrotThread.Execute;
begin
  if FImage <> nil then
    FImage.ReCalcColors;
end;

procedure TCnMandelbrotThread.NotifyProgress;
begin
  if FImage <> nil then
    FImage.DoProgress(FProgress, FTotal);
end;

procedure TCnMandelbrotThread.OnImageCalcEvent(Sender: TObject; Progress,
  Total: Integer; var AbortCalc: Boolean);
begin
  // �߳��ڵõ�����֪ͨ
  if Terminated then
    AbortCalc := True
  else
  begin
    FProgress := Progress;
    FTotal := Total;
    Synchronize(NotifyProgress);
  end;
end;

initialization
  Decimal4 := TCnBigDecimal.Create;
  Decimal4.SetWord(4);
  Binary4 := TCnBigBinary.Create;
  Binary4.SetWord(4);

finalization
  TmpDXZ.Free;
  TmpDYZ.Free;
  TmpBXZ.Free;
  TmpBYZ.Free;
  Decimal4.Free;
  Binary4.Free;

end.
