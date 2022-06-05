unit Unit25519;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, CnECC, Cn25519;

type
  TForm25519 = class(TForm)
    pgc25519: TPageControl;
    ts25519: TTabSheet;
    grp25519: TGroupBox;
    btnCurve25519G: TButton;
    btnEd25519G: TButton;
    btnCurve25519GAdd: TButton;
    btnEd25519GAdd: TButton;
    btnCurve25519GSub: TButton;
    btnEd25519GSub: TButton;
    btnCurve25519GMul: TButton;
    btnEd25519GMul: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCurve25519GClick(Sender: TObject);
    procedure btnEd25519GClick(Sender: TObject);
    procedure btnCurve25519GAddClick(Sender: TObject);
    procedure btnEd25519GAddClick(Sender: TObject);
    procedure btnCurve25519GSubClick(Sender: TObject);
    procedure btnEd25519GSubClick(Sender: TObject);
    procedure btnCurve25519GMulClick(Sender: TObject);
    procedure btnEd25519GMulClick(Sender: TObject);
  private
    FCurve25519: TCnCurve25519;
    FEd25519: TCnEd25519;
  public

  end;

var
  Form25519: TForm25519;

implementation

{$R *.DFM}

procedure TForm25519.FormCreate(Sender: TObject);
begin
  FCurve25519 := TCnCurve25519.Create;
  FEd25519 := TCnEd25519.Create;
end;

procedure TForm25519.FormDestroy(Sender: TObject);
begin
  FEd25519.Free;
  FCurve25519.Free;
end;

procedure TForm25519.btnCurve25519GClick(Sender: TObject);
begin
  if FCurve25519.IsPointOnCurve(FCurve25519.Generator) then
    ShowMessage('Curve 25519 Generator Point is on this Curve');
end;

procedure TForm25519.btnEd25519GClick(Sender: TObject);
begin
  if FEd25519.IsPointOnCurve(FEd25519.Generator) then
    ShowMessage('Ed 25519 Generator Point is on this Curve');
end;

procedure TForm25519.btnCurve25519GAddClick(Sender: TObject);
var
  P, Q: TCnEccPoint;
begin
  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;

  P.Assign(FCurve25519.Generator);
  Q.Assign(FCurve25519.Generator);

  if FCurve25519.PointAddPoint(P, Q, P) then
  begin
    // P 是 2*G
    if FCurve25519.IsPointOnCurve(P) then
      ShowMessage('Curve 25519 G + G is on this Curve');
  end;

  if FCurve25519.PointAddPoint(P, Q, P) then
  begin
    // P 是 3*G
    if FCurve25519.IsPointOnCurve(P) then
      ShowMessage('Curve 25519 G + 2*G is on this Curve');
  end;

  P.Assign(FCurve25519.Generator);
  Q.Assign(FCurve25519.Generator);
  FCurve25519.PointInverse(Q);

  if FCurve25519.PointAddPoint(P, Q, P) then
    if P.IsZero then
      ShowMessage('Curve 25519 G + -G is Zero');

  Q.Free;
  P.Free;
end;

procedure TForm25519.btnEd25519GAddClick(Sender: TObject);
var
  P, Q: TCnEccPoint;
begin
  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;

  P.Assign(FEd25519.Generator);
  Q.Assign(FEd25519.Generator);

  if FEd25519.PointAddPoint(P, Q, P) then
  begin
    // P 是 2*G
    if FEd25519.IsPointOnCurve(P) then
      ShowMessage('Curve 25519 G + G is on this Curve');
  end;

  if FEd25519.PointAddPoint(P, Q, P) then
  begin
    // P 是 3*G
    if FEd25519.IsPointOnCurve(P) then
      ShowMessage('Ed 25519 G + 2*G is on this Curve');
  end;

  P.Assign(FEd25519.Generator);
  Q.Assign(FEd25519.Generator);
  FEd25519.PointInverse(Q);

  if FEd25519.PointAddPoint(P, Q, P) then
    if FEd25519.IsNeutualPoint(P) then
      ShowMessage('Ed 25519 G + -G is Zero');

  Q.Free;
  P.Free;
end;

procedure TForm25519.btnCurve25519GSubClick(Sender: TObject);
var
  P, Q: TCnEccPoint;
begin
  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;

  P.Assign(FCurve25519.Generator);
  Q.Assign(FCurve25519.Generator);

  FCurve25519.PointAddPoint(P, Q, P);
  FCurve25519.PointAddPoint(P, Q, P);

  // P 是 3*G，Q 是 G
  if FCurve25519.PointSubPoint(P, Q, Q) then // Q 是 2*G
    if FCurve25519.IsPointOnCurve(Q) then
      ShowMessage('Curve 25519 3*G - G is on this Curve');

  if FCurve25519.PointSubPoint(P, P, Q) then // Q 是无限远点
    if Q.IsZero then
      ShowMessage('Curve 25519 G - G is Zero');

  Q.Free;
  P.Free;
end;

procedure TForm25519.btnEd25519GSubClick(Sender: TObject);
var
  P, Q: TCnEccPoint;
begin
  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;

  P.Assign(FEd25519.Generator);
  Q.Assign(FEd25519.Generator);

  FEd25519.PointAddPoint(P, Q, P);
  FEd25519.PointAddPoint(P, Q, P);

  // P 是 3*G，Q 是 G
  if FEd25519.PointSubPoint(P, Q, Q) then // Q 是 2*G
    if FEd25519.IsPointOnCurve(Q) then
      ShowMessage('Curve 25519 3*G - G is on this Curve');

  if FEd25519.PointSubPoint(P, P, Q) then // Q 是无限远点
    if FEd25519.IsNeutualPoint(Q) then
      ShowMessage('Curve 25519 G - G is Zero');

  Q.Free;
  P.Free;
end;

procedure TForm25519.btnCurve25519GMulClick(Sender: TObject);
var
  P: TCnEccPoint;
begin
  P := TCnEccPoint.Create;
  P.Assign(FCurve25519.Generator);

  FCurve25519.MultiplePoint(456768997823, P);
  if FCurve25519.IsPointOnCurve(P) then
    ShowMessage('Curve 25519 Random * G is on this Curve');

  P.Free;
end;

procedure TForm25519.btnEd25519GMulClick(Sender: TObject);
var
  P: TCnEccPoint;
begin
  P := TCnEccPoint.Create;
  P.Assign(FEd25519.Generator);

  FEd25519.MultiplePoint(8099261456373487672, P);
  if FEd25519.IsPointOnCurve(P) then
    ShowMessage('Ed 25519 Random * G is on this Curve');

  P.Free;
end;

end.
