unit UnitBinaryTree;

interface

uses
  {$IFDEF MSWINDOWS} Windows, Messages, {$ENDIF} SysUtils, Classes, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.ExtCtrls, CnTree, CnTreeClasses, FMX.Objects, FMX.Types, System.Types, System.UITypes;

type
  TDrawLeafEvent = procedure (Tree: TCnTree; ACanvas: TCanvas; X, Y: Integer;
    Leaf: TCnBinaryLeaf) of object;

  TFormBinaryTree = class(TForm)
    pbBinaryTree: TPaintBox;
    procedure pbBinaryTreePaint(Sender: TObject; Canvas: TCanvas);
  private
    FTreeRef: TCnBinaryTree;
    FTreeHeight: Integer;
    FOnDrawLeaf: TDrawLeafEvent;
    procedure SetTreeRef(const Value: TCnBinaryTree);
    procedure DrawLeaf(ACanvas: TCanvas; X, Y: Integer; Leaf: TCnBinaryLeaf);
  public
    property TreeRef: TCnBinaryTree read FTreeRef write SetTreeRef;
    property OnDrawLeaf: TDrawLeafEvent read FOnDrawLeaf write FOnDrawLeaf;
  end;

var
  FormBinaryTree: TFormBinaryTree;

implementation

{$R *.fmx}

const
  MARGIN = 50;
  RADIUS = 20;

var
  XOFFSET: Integer = 50; // 下一层横向偏移
  YOFFSET: Integer = 50; // 下一层竖向偏移

procedure TFormBinaryTree.DrawLeaf(ACanvas: TCanvas; X, Y: Integer;
  Leaf: TCnBinaryLeaf);
var
  X1, Y1: Integer;
  P1, P2: TPointF;
begin
  // 画一个节点，并画指向子节点的线，并递归调用画子节点
  if Leaf <> nil then
  begin
    if Leaf.LeftLeaf <> nil then
    begin
      X1 := X - (XOFFSET + (FTreeHeight - Leaf.Level) * 10);
      Y1 := Y + YOFFSET;
      P1.X := X;
      P1.Y := Y;
      P2.X := X1;
      P2.Y := Y1;
      ACanvas.DrawLine(P1, P2, 1);
      DrawLeaf(ACanvas, X1, Y1, Leaf.LeftLeaf);
    end;
    if Leaf.RightLeaf <> nil then
    begin

      X1 := X + (XOFFSET + (FTreeHeight - Leaf.Level) * 10);
      Y1 := Y + YOFFSET;
      P1.X := X;
      P1.Y := Y;
      P2.X := X1;
      P2.Y := Y1;
      ACanvas.DrawLine(P1, P2, 1);
      DrawLeaf(ACanvas, X1, Y1, Leaf.RightLeaf);
    end;
    ACanvas.DrawEllipse(RectF(X - RADIUS, Y - RADIUS, X + RADIUS, Y + RADIUS), 1);
    if Assigned(FOnDrawLeaf) then
      FOnDrawLeaf(TreeRef, ACanvas, X, Y, Leaf);
  end;
end;

procedure TFormBinaryTree.pbBinaryTreePaint(Sender: TObject; Canvas: TCanvas);
var
  L, X, Y: Integer;
begin
  pbBinaryTree.Canvas.Fill.Color := TColorRec.White;
  pbBinaryTree.Canvas.FillRect(RectF(0, 0, pbBinaryTree.Width, pbBinaryTree.Height),
    1, 1, [], 1);

  if TreeRef = nil then
    Exit;

  L := TreeRef.MaxLevel + 1; // 有 L 层节点，L - 1 层线
  X := Trunc(pbBinaryTree.Width / 2);
  Y := MARGIN;
  FTreeHeight := L;

  if L > 1 then
  begin
    XOFFSET := (X - L * MARGIN) div (L - 1);
    YOFFSET := Trunc((pbBinaryTree.Height - 2 * MARGIN) / (L - 1));
  end;
  DrawLeaf(pbBinaryTree.Canvas, X, Y, TreeRef.Root);
end;

procedure TFormBinaryTree.SetTreeRef(const Value: TCnBinaryTree);
begin
  FTreeRef := Value;
  pbBinaryTree.Repaint;
end;

end.
