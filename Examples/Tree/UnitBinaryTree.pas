unit UnitBinaryTree;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, CnTree, CnTreeClasses;

type
  TDrawLeafEvent = procedure (Tree: TCnTree; ACanvas: TCanvas; X, Y: Integer;
    Leaf: TCnBinaryLeaf) of object;

  TFormBinaryTree = class(TForm)
    pbBinaryTree: TPaintBox;
    procedure pbBinaryTreePaint(Sender: TObject);
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

{$R *.DFM}

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
begin
  // 画一个节点，并画指向子节点的线，并递归调用画子节点
  if Leaf <> nil then
  begin
    if Leaf.LeftLeaf <> nil then
    begin
      ACanvas.MoveTo(X, Y);
      X1 := X - (XOFFSET + (FTreeHeight - Leaf.Level) * 10);
      Y1 := Y + YOFFSET;
      ACanvas.LineTo(X1, Y1);
      DrawLeaf(ACanvas, X1, Y1, Leaf.LeftLeaf);
    end;
    if Leaf.RightLeaf <> nil then
    begin
      ACanvas.MoveTo(X, Y);
      X1 := X + (XOFFSET + (FTreeHeight - Leaf.Level) * 10);
      Y1 := Y + YOFFSET;
      ACanvas.LineTo(X1, Y1);
      DrawLeaf(ACanvas, X1, Y1, Leaf.RightLeaf);
    end;
    ACanvas.Ellipse(Rect(X - RADIUS, Y - RADIUS, X + RADIUS, Y + RADIUS));
    if Assigned(FOnDrawLeaf) then
      FOnDrawLeaf(TreeRef, ACanvas, X, Y, Leaf);
  end;
end;

procedure TFormBinaryTree.pbBinaryTreePaint(Sender: TObject);
var
  L, X, Y: Integer;
begin
  pbBinaryTree.Canvas.Brush.Color := clWhite;
  pbBinaryTree.Canvas.FillRect(Rect(0, 0, pbBinaryTree.Width, pbBinaryTree.Height));

  if TreeRef = nil then
    Exit;

  L := TreeRef.MaxLevel + 1; // 有 L 层节点，L - 1 层线
  X := pbBinaryTree.Width div 2;
  Y := MARGIN;
  FTreeHeight := L;

  if L > 1 then
  begin
    XOFFSET := (X - L * MARGIN) div (L - 1);
    YOFFSET := (pbBinaryTree.Height - 2 * MARGIN) div (L - 1);
  end;
  DrawLeaf(pbBinaryTree.Canvas, X, Y, TreeRef.Root);
end;

procedure TFormBinaryTree.SetTreeRef(const Value: TCnBinaryTree);
begin
  FTreeRef := Value;
  pbBinaryTree.Invalidate;
end;

end.
