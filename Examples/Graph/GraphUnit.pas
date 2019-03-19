unit GraphUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, CnGraph;

type
  TFormGraph = class(TForm)
    pgc1: TPageControl;
    tsDirected: TTabSheet;
    mmoLoad: TMemo;
    btnLoad: TButton;
    btnDumpVertex: TButton;
    btnBiCheck: TButton;
    lstVertex: TListBox;
    mmoTravel: TMemo;
    mmoEdge: TMemo;
    btnAdjMatrix: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnDumpVertexClick(Sender: TObject);
    procedure btnBiCheckClick(Sender: TObject);
    procedure lstVertexDblClick(Sender: TObject);
    procedure lstVertexClick(Sender: TObject);
    procedure btnAdjMatrixClick(Sender: TObject);
  private
    FDirectedGraph: TCnGraph;
    FTravelResults: TStringList;
    procedure OnTravel(Vertex: TCnVertex);
  public
    { Public declarations }
  end;

var
  FormGraph: TFormGraph;

implementation

{$R *.DFM}

procedure TFormGraph.FormCreate(Sender: TObject);
begin
  FDirectedGraph := TCnGraph.Create(True);
  FDirectedGraph.OnWidthFirstTravelVertex := OnTravel;
  FTravelResults := TStringList.Create;
end;

procedure TFormGraph.FormDestroy(Sender: TObject);
begin
  FTravelResults.Free;
  FDirectedGraph.Free;
end;

procedure TFormGraph.btnLoadClick(Sender: TObject);
var
  I: Integer;
  S, V1, V2: string;
begin
  FDirectedGraph.ClearVertexes;
  I := 0;

  while I < mmoLoad.Lines.Count do
  begin
    S := mmoLoad.Lines[I];
    if Length(S) = 0 then
    begin
      Inc(I);
      Continue;
    end;

    if S[1] <> ' ' then
    begin
      V1 := S;
      if FDirectedGraph.FindVertex(V1) = nil then
        FDirectedGraph.AddVertex(V1);
    end
    else
    begin
      V2 := Trim(S);
      FDirectedGraph.AddVertexesEdge(V1, V2);
    end;
    Inc(I);
  end;

  ShowMessage(Format('Directed Graph Vertexes %d, Edges %d',
    [FDirectedGraph.VertexCount, FDirectedGraph.EdgeCount]));
end;

procedure TFormGraph.btnDumpVertexClick(Sender: TObject);
var
  I: Integer;
begin
  lstVertex.Items.Clear;
  for I := 0 to FDirectedGraph.VertexCount - 1 do
    lstVertex.Items.Add(FDirectedGraph.Vertex[I].Text);

  ShowMessage('Dump Vertex Count: ' + IntToStr(lstVertex.Items.Count));
end;

procedure TFormGraph.btnBiCheckClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to mmoLoad.Lines.Count - 1 do
  begin
    if lstVertex.Items.IndexOf(Trim(mmoLoad.Lines[I])) < 0 then
      ShowMessage('Left NOT Found in Right: ' + mmoLoad.Lines[I]);
  end;

  for I := 0 to lstVertex.Items.Count - 1 do
  begin
    if mmoLoad.Lines.IndexOf(lstVertex.Items[I]) < 0 then
      ShowMessage('Right NOT Found in Left: ' + lstVertex.Items[I]);
  end;
end;

procedure TFormGraph.OnTravel(Vertex: TCnVertex);
begin
  if Vertex <> nil then
    FTravelResults.Add(Vertex.Text);
end;

procedure TFormGraph.lstVertexDblClick(Sender: TObject);
var
  V: TCnVertex;
begin
  mmoTravel.Lines.Clear;
  FTravelResults.Clear;
  V := FDirectedGraph.FindVertex(lstVertex.Items[lstVertex.ItemIndex]);
  if V <> nil then
    FDirectedGraph.WidthFirstTravel(V);
  mmoTravel.Lines.Text := FTravelResults.Text;
  ShowMessage('Get Width Travel Count ' + IntToStr(mmoTravel.Lines.Count));
end;

procedure TFormGraph.lstVertexClick(Sender: TObject);
var
  I: Integer;
  V: TCnVertex;
begin
  mmoEdge.Lines.Clear;
  V := FDirectedGraph.FindVertex(lstVertex.Items[lstVertex.ItemIndex]);
  if V <> nil then
  begin
    for I := 0 to V.OutNeighbourCount - 1 do
      mmoEdge.Lines.Add(V.OutNeighbour[I].Text);
  end;
end;

procedure TFormGraph.btnAdjMatrixClick(Sender: TObject);
var
  M: TCnAdjacencyMatrix;
  List: TStrings;
begin
  M := FDirectedGraph.DumpToAdjacencyMatrix;
  List := TStringList.Create;
  CnMatrixToStrings(M, List);
  ShowMessage(List.Text);
  List.Free;
  SetLength(M, 0);
end;

end.
