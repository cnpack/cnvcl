{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2021 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnGraph;
{* |<PRE>
================================================================================
* 软件名称：CnPack 公共单元
* 单元名称：实现图的单元
* 单元作者：刘啸 (liuxiao@cnpack.org)
* 备    注：
* 开发平台：Win 7 + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2019.03.19 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs;

type
  TCnGraph = class;

  ECnGraphException = class(Exception);

  TCnGraphMatrix = array of array of Integer;

  TCnAdjacencyMatrix = TCnGraphMatrix;
  {* 邻接矩阵，顶点与顶点}

  TCnIncidenceMatrix = TCnGraphMatrix;
  {* 关联矩阵，顶点与边}

  TCnVertex = class(TObject)
  {* 顶点实现类}
  private
    FOutNeighbours: TObjectList;
    FWeights: TList;
    FInNeighbours: TObjectList;
    FText: string;
    FOwner: TCnGraph;
    FData: TObject;
    FVisited: Boolean;
    function GetOutNeighbourCount: Integer;
    function GetInNeighbourCount: Integer;
    function GetInNeighbour(Index: Integer): TCnVertex;
    function GetOutNeighbour(Index: Integer): TCnVertex;
    function GetWeight(Index: Integer): Integer;
  protected
    procedure AddInNeighbour(PrevRef: TCnVertex);
    {* 单纯添加一个顶点到 InNeighbour}
    procedure RemoveInNeighbour(PrevRef: TCnVertex);
    {* 单纯从 InNeighbour 中删除一个顶点}
    property Visited: Boolean read FVisited write FVisited;
    {* 是否被访问过的标记，用于图的遍历}
  public
    constructor Create(AOwner: TCnGraph); virtual;
    destructor Destroy; override;

    procedure AddOutNeighbour(NextRef: TCnVertex; Weight: Integer = 1);
    {* 添加出的相邻点与权重，内部会把 Self 添加到 NextRef 的 InNeighbour 中}
    procedure RemoveOutNeighbour(NextRef: TCnVertex);
    {* 删除出的相邻点}
    procedure ClearNeighbours;
    {* 清除所有出入相邻的顶点}

    property Owner: TCnGraph read FOwner;
    {* 所属的图}

    property Text: string read FText write FText;
    {* 存储文本属性}
    property Data: TObject read FData write FData;
    {* 存储数据对象属性}

    property Weight[Index: Integer]: Integer read GetWeight;
    {* 以此顶点为起点的边的权重}
    property OutNeighbour[Index: Integer]: TCnVertex read GetOutNeighbour;
    {* 以此顶点为起点的相邻顶点}
    property OutNeighbourCount: Integer read GetOutNeighbourCount;
    {* 以此顶点为起点的边的数量，也即出度}

    property InNeighbour[Index: Integer]: TCnVertex read GetInNeighbour;
    {* 以此顶点为终点的相邻顶点}
    property InNeighbourCount: Integer read GetInNeighbourCount;
    {* 以此顶点为终点的边的数量，也即入度}
  end;

  TCnGraphTravelEvent = procedure(Vertex: TCnVertex) of object;
  {* 遍历图时触发访问某顶点的事件，Vertex 是顶点}

  TCnGraph = class(TObject)
  {* 图实现类}
  private
    FVertexes: TObjectList;
    FDirected: Boolean;
    FEdgeCount: Integer;
    FOnDepthFirstTravelVertex: TCnGraphTravelEvent;
    FOnWidthFirstTravelVertex: TCnGraphTravelEvent;
    function GetVertexCount: Integer;
    function GetVertex(Index: Integer): TCnVertex;
  protected
    procedure DoDepthFirstTravel(Vertex: TCnVertex);
    procedure DoWidthFirstTravel(Vertex: TCnVertex);
  public
    constructor Create(ADirected: Boolean = True); virtual;
    {* 构造函数，默认是有向图}
    destructor Destroy; override;
    {* 析构函数}

    function HasVertex(Vertex: TCnVertex): Boolean;
    {* 图里是否有指定顶点}

    function AddVertex(const Text: string): TCnVertex;
    {* 添加孤立顶点}
    function FindVertex(const Text: string): TCnVertex;
    {* 查找 Text 为指定内容的顶点，如有重复，只返回第一个}

    function AddEdge(Vertex1, Vertex2: TCnVertex; Weight: Integer = 1): Boolean;
    {* 添加一条边。如果是有向图，则添加 Vertex1 指向 Vertex2 的边，
      无向图添加 Vertex1 指向 Vertex2 边与 Vertex2 指向 Vertex1 边 }
    function AddVertexesEdge(const Text1, Text2: string; Weight: Integer = 1): Boolean;
    {* 按 Text 添加一条边。如果已存在 Text 相同的顶点则以顶点为准，否则先添加顶点再添加边}
    function RemoveEdge(Vertex1, Vertex2: TCnVertex): Boolean;
    {* 删除一条边。如果是有向图，则删除 Vertex1 指向 Vertex2 的边，
      无向图删除 Vertex1 指向 Vertex2 边与 Vertex2 指向 Vertex1 边}
    function RemoveVertex(Vertex: TCnVertex): Boolean;
    {* 删除一个顶点以及以它关联的所有边并 Free 这个顶点}

    function GetVertexOutDegree(Vertex: TCnVertex): Integer;
    {* 有向图情况下得到某顶点的出度}
    function GetVertexInDegree(Vertex: TCnVertex): Integer;
    {* 有向图情况下得到某顶点的入度}
    function GetVertexDegree(Vertex: TCnVertex): Integer;
    {* 得到某顶点的度，有向图情况下也即出入度之和}

    procedure ClearVertexes;
    {* 清除所有顶点，顺便也就清除了所有边}
    procedure ClearEdges;
    {* 清除所有边}
    procedure ClearVisited;
    {* 清除访问标记}

    function DumpToAdjacencyMatrix: TCnAdjacencyMatrix;
    {* 将内容输出到邻接矩阵}
    function DumpToIncidenceMatrix: TCnIncidenceMatrix;
    {* 将内容输出到关联矩阵}
    procedure DepthFirstTravel(Vertex: TCnVertex);
    {* 从某顶点出发进行深度优先遍历 }
    procedure WidthFirstTravel(Vertex: TCnVertex);
    {* 从某顶点出发进行广度优先遍历 }

    property Directed: Boolean read FDirected;
    {* 是否是有向图}
    property EdgeCount: Integer read FEdgeCount;
    {* 边数量}
    property Vertex[Index: Integer]: TCnVertex read GetVertex;
    {* 顶点列表}
    property VertexCount: Integer read GetVertexCount;
    {* 顶点数量}

    property OnDepthFirstTravelVertex: TCnGraphTravelEvent
      read FOnDepthFirstTravelVertex write FOnDepthFirstTravelVertex;
    {* 深度优先遍历时遍历到一个顶点时的触发事件，Sender 是此顶点 }
    property OnWidthFirstTravelVertex: TCnGraphTravelEvent
      read FOnWidthFirstTravelVertex write FOnWidthFirstTravelVertex;
    {* 广度优先遍历时遍历到一个顶点时的触发事件，Sender 是此顶点 }
  end;

procedure CnMatrixToStrings(Matrix: TCnGraphMatrix; List: TStrings);
{* 将矩阵转换为字符串列表用来显示}

implementation

procedure CnMatrixToStrings(Matrix: TCnGraphMatrix; List: TStrings);
var
  I, J: Integer;
  S: string;
begin
  if (Matrix <> nil) and (List <> nil) then
  begin
    List.Clear;
    for I := Low(Matrix) to High(Matrix) do
    begin
      S := '';
      for J := Low(Matrix[I]) to High(Matrix[I]) do
        S := S + ' ' + Format('%2d', [Matrix[I, J]]);
      List.Add(S);
    end;
  end;
end;

{ TCnVertex }

procedure TCnVertex.AddInNeighbour(PrevRef: TCnVertex);
begin
  if PrevRef <> nil then
    FInNeighbours.Add(PrevRef);
end;

procedure TCnVertex.AddOutNeighbour(NextRef: TCnVertex; Weight: Integer);
begin
  if NextRef <> nil then
  begin
    FOutNeighbours.Add(NextRef);
    FWeights.Add(Pointer(Weight));
    NextRef.AddInNeighbour(Self);
  end;
end;

procedure TCnVertex.ClearNeighbours;
begin
  FOutNeighbours.Clear;
  FInNeighbours.Clear;
  FWeights.Clear;
end;

constructor TCnVertex.Create(AOwner: TCnGraph);
begin
  inherited Create;
  FOwner := AOwner;
  FOutNeighbours := TObjectList.Create(False);
  FWeights := TList.Create;
  FInNeighbours := TObjectList.Create(False);
end;

destructor TCnVertex.Destroy;
begin
  FInNeighbours.Free;
  FWeights.Free;
  FOutNeighbours.Free;
  inherited;
end;

function TCnVertex.GetInNeighbour(Index: Integer): TCnVertex;
begin
  Result := FInNeighbours[Index] as TCnVertex;
end;

function TCnVertex.GetInNeighbourCount: Integer;
begin
  Result := FInNeighbours.Count;
end;

function TCnVertex.GetOutNeighbour(Index: Integer): TCnVertex;
begin
  Result := FOutNeighbours[Index] as TCnVertex;
end;

function TCnVertex.GetOutNeighbourCount: Integer;
begin
  Result := FOutNeighbours.Count;
end;

function TCnVertex.GetWeight(Index: Integer): Integer;
begin
  Result := Integer(FWeights[Index]);
end;

procedure TCnVertex.RemoveInNeighbour(PrevRef: TCnVertex);
begin
  if PrevRef <> nil then
    FInNeighbours.Remove(PrevRef);
end;

procedure TCnVertex.RemoveOutNeighbour(NextRef: TCnVertex);
var
  WeightIndex: Integer;
begin
  if NextRef <> nil then
  begin
    WeightIndex := FOutNeighbours.IndexOf(NextRef);
    if WeightIndex >= 0 then
    begin
      FOutNeighbours.Delete(WeightIndex);
      FWeights.Delete(WeightIndex);
      NextRef.RemoveInNeighbour(Self);
    end;
  end;
end;

{ TCnGraph }

function TCnGraph.AddEdge(Vertex1, Vertex2: TCnVertex; Weight: Integer): Boolean;
begin
  Result := False;

  if not HasVertex(Vertex1) or not HasVertex(Vertex2) then
    Exit;

  Vertex1.AddOutNeighbour(Vertex2, Weight);
  if not FDirected and (Vertex1 <> Vertex2) then
    Vertex2.AddOutNeighbour(Vertex1, Weight);

  Inc(FEdgeCount);
  Result := True;
end;

function TCnGraph.AddVertex(const Text: string): TCnVertex;
begin
  Result := TCnVertex.Create(Self);
  Result.Text := Text;
  FVertexes.Add(Result);
end;

function TCnGraph.AddVertexesEdge(const Text1, Text2: string;
  Weight: Integer): Boolean;
var
  V1, V2: TCnVertex;
begin
  V1 := FindVertex(Text1);
  if V1 = nil then
    V1 := AddVertex(Text1);

  V2 := FindVertex(Text2);
  if V2 = nil then
    V2 := AddVertex(Text2);

  Result := AddEdge(V1, V2, Weight);
end;

procedure TCnGraph.ClearEdges;
var
  I: Integer;
begin
  for I := 0 to FVertexes.Count - 1 do
    TCnVertex(FVertexes[I]).ClearNeighbours;
  FEdgeCount := 0;
end;

procedure TCnGraph.ClearVertexes;
begin
  FVertexes.Clear;
  FEdgeCount := 0;
end;

procedure TCnGraph.ClearVisited;
var
  I: Integer;
begin
  for I := 0 to FVertexes.Count - 1 do
    TCnVertex(FVertexes[I]).Visited := False;
end;

constructor TCnGraph.Create(ADirected: Boolean);
begin
  inherited Create;
  FDirected := ADirected;
  FVertexes := TObjectList.Create(True);
end;

procedure TCnGraph.DepthFirstTravel(Vertex: TCnVertex);
begin
  if HasVertex(Vertex) then
  begin
    ClearVisited;
    DoDepthFirstTravel(Vertex);
  end;
end;

destructor TCnGraph.Destroy;
begin
  FVertexes.Free;
  inherited;
end;

procedure TCnGraph.DoDepthFirstTravel(Vertex: TCnVertex);
var
  I: Integer;
begin
  Vertex.Visited := True;
  if Assigned(FOnDepthFirstTravelVertex) then
    FOnDepthFirstTravelVertex(Vertex);

  for I := 0 to Vertex.OutNeighbourCount - 1 do
    if not Vertex.OutNeighbour[I].Visited then
      DoDepthFirstTravel(Vertex.OutNeighbour[I]);
end;

procedure TCnGraph.DoWidthFirstTravel(Vertex: TCnVertex);
var
  Queue: TQueue;
  I: Integer;
  V: TCnVertex;
begin
  Queue := TQueue.Create;
  try
    Vertex.Visited := True;
    Queue.Push(Vertex);

    while Queue.Count > 0 do
    begin
      V := TCnVertex(Queue.Pop);
      if Assigned(FOnWidthFirstTravelVertex) then
        FOnWidthFirstTravelVertex(V);

      for I := 0 to V.OutNeighbourCount - 1 do
      begin
        if not V.OutNeighbour[I].Visited then
        begin
          V.OutNeighbour[I].Visited := True;
          Queue.Push(V.OutNeighbour[I]);
        end;
      end;
    end;
  finally
    Queue.Free;
  end;   
end;

function TCnGraph.DumpToAdjacencyMatrix: TCnAdjacencyMatrix;
var
  Row, Col, Idx: Integer;
  VR, VC: TCnVertex;
begin
  if VertexCount = 0 then
    raise ECnGraphException.Create('NO Vertexes.');

  SetLength(Result, VertexCount, VertexCount);

  for Row := 0 to FVertexes.Count - 1 do
  begin
    VR := TCnVertex(FVertexes[Row]);
    for Col := 0 to VR.OutNeighbourCount - 1 do
    begin
      VC := VR.OutNeighbour[Col];
      Idx := FVertexes.IndexOf(VC);
      if Idx >= 0 then
        Result[Row, Idx] := VR.Weight[Col];
    end;
  end;
end;

function TCnGraph.DumpToIncidenceMatrix: TCnIncidenceMatrix;
var
  Row, Col, I, Idx: Integer;
begin
  if VertexCount = 0 then
    raise ECnGraphException.Create('NO Vertexes.');
  if EdgeCount = 0 then
    raise ECnGraphException.Create('NO Edges.');

  SetLength(Result, VertexCount, EdgeCount);

  // 导出至关联矩阵，无权重，无法处理有向图中的顶点自环
  Col := 0;
  for Row := 0 to FVertexes.Count - 1 do
  begin
    for I := 0 to Vertex[Row].OutNeighbourCount - 1 do
    begin
      // 找到一条边，起始点是 Vertex[Row] 以及 Vertex[Row].OutNeighbour[I]
      Idx := FVertexes.IndexOf(Vertex[Row].OutNeighbour[I]);
      if Idx > 0 then
      begin
        Result[Row, Col] := 1;  // 边起点
        if FDirected then
        begin
          if Result[Idx, Col] = 0 then // 不是自指边
            Result[Idx, Col] := -1     // 边终点
        end
        else
          Result[Idx, Col] := 1;  // 无向图都用 1
      end;
      Inc(Col);
    end;
  end;
end;

function TCnGraph.FindVertex(const Text: string): TCnVertex;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FVertexes.Count - 1 do
  begin
    if TCnVertex(FVertexes[I]).Text = Text then
    begin
      Result := TCnVertex(FVertexes[I]);
      Exit;
    end;
  end;
end;

function TCnGraph.GetVertex(Index: Integer): TCnVertex;
begin
  Result := TCnVertex(FVertexes[Index]);
end;

function TCnGraph.GetVertexCount: Integer;
begin
  Result := FVertexes.Count;
end;

function TCnGraph.GetVertexDegree(Vertex: TCnVertex): Integer;
begin
  if HasVertex(Vertex) then
  begin
    if FDirected then
      Result := Vertex.InNeighbourCount + Vertex.OutNeighbourCount
    else // 无向图以出度为准
      Result := Vertex.OutNeighbourCount;
  end
  else
    Result := -1;
end;

function TCnGraph.GetVertexInDegree(Vertex: TCnVertex): Integer;
begin
  if not FDirected then
    raise ECnGraphException.Create('NO InDegree for Undirected Graph.');

  if HasVertex(Vertex) then
    Result := Vertex.InNeighbourCount
  else
    Result := -1;
end;

function TCnGraph.GetVertexOutDegree(Vertex: TCnVertex): Integer;
begin
  if not FDirected then
    raise ECnGraphException.Create('NO OutDegree for Undirected Graph.');

  if HasVertex(Vertex) then
    Result := Vertex.OutNeighbourCount
  else
    Result := -1;
end;

function TCnGraph.HasVertex(Vertex: TCnVertex): Boolean;
begin
  Result := (Vertex <> nil) and (Vertex.Owner = Self) and (FVertexes.IndexOf(Vertex) >= 0);
end;

function TCnGraph.RemoveEdge(Vertex1, Vertex2: TCnVertex): Boolean;
begin
  Result := False;
  if not HasVertex(Vertex1) or not HasVertex(Vertex2) then
    Exit;

  Vertex1.RemoveOutNeighbour(Vertex2);
  if not FDirected and (Vertex1 <> Vertex2) then
    Vertex2.RemoveOutNeighbour(Vertex1);

  Dec(FEdgeCount);
  Result := True;
end;

function TCnGraph.RemoveVertex(Vertex: TCnVertex): Boolean;
var
  I: Integer;
begin
  Result := False;
  if not HasVertex(Vertex) then
    Exit;

  // 删 OutNeighbours 里每一个里的 InNeighbours 里的自己
  for I := 0 to Vertex.OutNeighbourCount - 1 do
    Vertex.OutNeighbour[I].RemoveInNeighbour(Vertex);

  // 删 InNeighbours 里每一个里的 OutNeighbours 里的自己
  for I := 0 to Vertex.InNeighbourCount - 1 do
    Vertex.InNeighbour[I].RemoveOutNeighbour(Vertex);

  Vertex.ClearNeighbours;
  FVertexes.Remove(Vertex);
  Vertex.Free;
  Result := True;
end;

procedure TCnGraph.WidthFirstTravel(Vertex: TCnVertex);
begin
  if HasVertex(Vertex) then
  begin
    ClearVisited;
    DoWidthFirstTravel(Vertex);
  end;
end;

end.
