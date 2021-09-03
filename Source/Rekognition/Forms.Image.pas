unit Forms.Image;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.Generics.Collections, AWS.Rekognition, System.Types,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, PNGImage, JPEG,
  Vcl.ExtCtrls;

type
  TImageForm = class(TForm)
    PaintBox1: TPaintBox;
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    FImage: TPicture;
    FTextDetections: TObjectList<TTextDetection>;
    ImgBounds: TRect;
    FCelebrities: TObjectList<TCelebrity>;
    procedure DrawImage;
    function AspectRatio: Double;
    function TargetAspectRatio: Double;
    function HRatio: Double;
    function VRatio: Double;
    function TargetControl: TControl;
    function ImgH(const Value: Double): Integer;
    function ImgV(const Value: Double): Integer;
    function CanvasRect(Box: TBoundingBox): TRect;
    function CanvasPolygon(Points: TObjectList<AWS.Rekognition.TPoint>): TArray<TPoint>;
    procedure DrawTextDetection(Canvas: TCanvas; Detection: TTextDetection);
    procedure DrawTextDetections(Canvas: TCanvas);
    procedure DrawCelebrity(Canvas: TCanvas; Celebrity: TCelebrity);
    procedure DrawCelebrities(Canvas: TCanvas);
    function FindTextDetection(X, Y: Integer): TTextDetection;
    function FindCelebrity(X, Y: Integer): TCelebrity;
    procedure ImageChange(Sender: TObject);
    procedure SetTextDetections(const Value: TObjectList<TTextDetection>);
    procedure SetCelebrities(const Value: TObjectList<TCelebrity>);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Image: TPicture read FImage;
    property TextDetections: TObjectList<TTextDetection> read FTextDetections write SetTextDetections;
    property Celebrities: TObjectList<TCelebrity> read FCelebrities write SetCelebrities;
  end;

var
  ImageForm: TImageForm;

implementation

{$R *.dfm}

{ TImageForm }

function TImageForm.AspectRatio: Double;
begin
  if Image.Height = 0 then
    Result := 1
  else
    Result := Image.Width / Image.Height;
end;

function TImageForm.CanvasPolygon(Points: TObjectList<AWS.Rekognition.TPoint>): TArray<TPoint>;
var
  I: Integer;
begin
  SetLength(Result, Points.Count);
  for I := 0 to Points.Count - 1 do
  begin
    Result[I].X := ImgH(Points[I].X);
    Result[I].Y := ImgV(Points[I].Y);
  end;
end;

function TImageForm.CanvasRect(Box: TBoundingBox): TRect;
begin
  Result.Left := ImgH(Box.Left);
  Result.Top := ImgV(Box.Top);
  Result.Right := ImgH(Box.Left + Box.Width);
  Result.Bottom := ImgV(Box.Top + Box.Height);
end;

constructor TImageForm.Create(AOwner: TComponent);
begin
  inherited;
  FImage := TPicture.Create;
  FImage.OnChange := ImageChange;
end;

destructor TImageForm.Destroy;
begin
  FImage.Free;
  TextDetections := nil;
  Celebrities := nil;
  inherited;
end;

procedure TImageForm.DrawCelebrities(Canvas: TCanvas);
var
  Celebrity: TCelebrity;
begin
  if Celebrities = nil then Exit;
  for Celebrity in Celebrities do
    DrawCelebrity(Canvas, Celebrity);
end;

procedure TImageForm.DrawImage;
var
  ImgHeight: Double;
  ImgWidth: Double;
begin
  if AspectRatio > TargetAspectRatio then
  begin
    ImgBounds.Left := 0;
    ImgBounds.Right := TargetControl.Width;
    ImgHeight := TargetControl.Width / AspectRatio;
    ImgBounds.Top := Round((TargetControl.Height - ImgHeight) / 2);
    ImgBounds.Bottom := Round(ImgBounds.Top + ImgHeight);
  end
  else
  begin
    ImgBounds.Top := 0;
    ImgBounds.Bottom := TargetControl.Height;
    ImgWidth := TargetControl.Height * AspectRatio;
    ImgBounds.Left := Round((TargetControl.Width - ImgWidth) / 2);
    ImgBounds.Right := Round(ImgBounds.Left + ImgWidth);
  end;
  Canvas.StretchDraw(ImgBounds, Image.Graphic);
end;

procedure TImageForm.DrawTextDetection(Canvas: TCanvas; Detection: TTextDetection);
begin
  Canvas.Pen.Width := 3;
  Canvas.Pen.Color := clBlue;
  Canvas.Brush.Style := bsClear;
  if Detection.IsSetGeometry then
    if Detection.Geometry.IsSetPolygon then
      Canvas.Polygon(CanvasPolygon(Detection.Geometry.Polygon))
    else
    if Detection.Geometry.IsSetBoundingBox then
      Canvas.Rectangle(CanvasRect(Detection.Geometry.BoundingBox));
end;

procedure TImageForm.DrawCelebrity(Canvas: TCanvas; Celebrity: TCelebrity);
begin
  Canvas.Pen.Width := 3;
  Canvas.Pen.Color := clGreen;
  Canvas.Brush.Style := bsClear;
  if Celebrity.IsSetFace and Celebrity.Face.IsSetBoundingBox then
    Canvas.Rectangle(CanvasRect(Celebrity.Face.BoundingBox));
end;

procedure TImageForm.DrawTextDetections(Canvas: TCanvas);
var
  TextDetection: TTextDetection;
begin
  if TextDetections = nil then Exit;
  for TextDetection in TextDetections do
    if not TextDetection.IsSetParentId then
      DrawTextDetection(Canvas, TextDetection);
end;

function TImageForm.FindCelebrity(X, Y: Integer): TCelebrity;
var
  Celebrity: TCelebrity;
begin
  if Celebrities = nil then Exit(nil);
  for Celebrity in Celebrities do
    if Celebrity.IsSetFace and Celebrity.Face.IsSetBoundingBox then
    begin
      if PtInRect(CanvasRect(Celebrity.Face.BoundingBox), Point(X, Y)) then
        Exit(Celebrity);
    end;
  Result := nil;
end;

function TImageForm.FindTextDetection(X, Y: Integer): TTextDetection;
var
  R: HRGN;
  Pts: TArray<TPoint>;
  TextDetection: TTextDetection;
begin
  if TextDetections = nil then Exit(nil);
  for TextDetection in TextDetections do
    if TextDetection.IsSetGeometry and TextDetection.Geometry.IsSetPolygon then
    begin
      Pts := CanvasPolygon(TextDetection.Geometry.Polygon);
      if Length(Pts) > 0 then
      begin
        R := CreatePolygonRgn(Pts[0], Length(Pts), ALTERNATE);
        try
          if PtInRegion(R, X, Y) then
            Exit(TextDetection);
        finally
          DeleteObject(R);
        end;
      end
    end;
  Result := nil;
end;

procedure TImageForm.FormPaint(Sender: TObject);
begin
  DrawImage;
  DrawTextDetections(Self.Canvas);
  DrawCelebrities(Self.Canvas);
end;

procedure TImageForm.FormResize(Sender: TObject);
begin
  Invalidate;
end;

function TImageForm.HRatio: Double;
begin
  Result := ImgBounds.Width / Image.Width;
end;

procedure TImageForm.ImageChange(Sender: TObject);
begin
  TextDetections := nil;
  Celebrities := nil;
  Invalidate;
end;

function TImageForm.ImgH(const Value: Double): Integer;
begin
  Result := Round(Image.Width * Value * HRatio + ImgBounds.Left);
end;

function TImageForm.ImgV(const Value: Double): Integer;
begin
  Result := Round(Image.Height * Value * VRatio + ImgBounds.Top);
end;

procedure TImageForm.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  TextDetection: TTextDetection;
  Celebrity: TCelebrity;
begin
  TextDetection := FindTextDetection(X, Y);
  Celebrity := FindCelebrity(X, Y);

  if TextDetection <> nil then
    Self.Hint := TextDetection.DetectedText
  else
  if Celebrity <> nil then
    Self.Hint := Celebrity.Name
  else
    Self.Hint := '';

  if Self.Hint <> '' then
    Application.ActivateHint(TargetControl.ClientToScreen(Point(X, Y)))
  else
    Application.HideHint;
end;

procedure TImageForm.SetCelebrities(const Value: TObjectList<TCelebrity>);
begin
  if FCelebrities <> Value then
  begin
    FCelebrities.Free;
    FCelebrities := Value;
    Invalidate;
  end;
end;

procedure TImageForm.SetTextDetections(const Value: TObjectList<TTextDetection>);
begin
  if FTextDetections <> Value then
  begin
    FTextDetections.Free;
    FTextDetections := Value;
    Invalidate;
  end;
end;

function TImageForm.TargetControl: TControl;
begin
//  Result := Self;
  Result := PaintBox1;
end;

function TImageForm.TargetAspectRatio: Double;
begin
  Result := TargetControl.Width / TargetControl.Height;
end;

function TImageForm.VRatio: Double;
begin
  Result := ImgBounds.Height / Image.Height;
end;

end.
