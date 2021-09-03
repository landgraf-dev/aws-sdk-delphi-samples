unit Forms.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.IOUtils, AWS.Rekognition, Vcl.Graphics, PNGImage, JPEG,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ExtDlgs, Vcl.StdCtrls;

type
  TForm8 = class(TForm)
    Button1: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    Button2: TButton;
    Button3: TButton;
    lbText: TListBox;
    lbCelebrities: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FClient: IAmazonRekognition;
    procedure LoadImage(const FileName: string);
    function Client: IAmazonRekognition;
    function CreateAWSImage: AWS.Rekognition.TImage;
  public
  end;

var
  Form8: TForm8;

implementation

{$R *.dfm}

uses Forms.Image;

procedure TForm8.Button1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    LoadImage(OpenPictureDialog1.FileName);
end;

procedure TForm8.Button2Click(Sender: TObject);
var
  Request: IDetectTextRequest;
  Response: IDetectTextResponse;
  TextDetection: TTextDetection;
begin
  Request := TDetectTextRequest.Create;
  Request.Image := CreateAWSImage;
  Response := Client.DetectText(Request);
  for TextDetection in Response.TextDetections do
    if not TextDetection.IsSetParentId then
      lbText.AddItem(TextDetection.DetectedText, TextDetection);
  Response.KeepTextDetections := True;
  ImageForm.TextDetections := Response.TextDetections;
end;

procedure TForm8.Button3Click(Sender: TObject);
var
  Request: IRecognizeCelebritiesRequest;
  Response: IRecognizeCelebritiesResponse;
  Celebrity: TCelebrity;
begin
  Request := TRecognizeCelebritiesRequest.Create;
  Request.Image := CreateAWSImage;
  Response := Client.RecognizeCelebrities(Request);
  for Celebrity in Response.CelebrityFaces do
    lbCelebrities.AddItem(Celebrity.Name, Celebrity);
  Response.KeepCelebrityFaces := True;
  ImageForm.Celebrities := Response.CelebrityFaces;
end;

function TForm8.Client: IAmazonRekognition;
begin
  if FClient = nil then
    FClient := TAmazonRekognitionClient.Create;
  Result := FClient;
end;

function TForm8.CreateAWSImage: AWS.Rekognition.TImage;
begin
  Result := AWS.Rekognition.TImage.Create;
  Result.Bytes := TBytesStream.Create;
  ImageForm.Image.SaveToStream(Result.Bytes);
  Result.Bytes.Position := 0;
end;

procedure TForm8.FormShow(Sender: TObject);
var
  FileName: string;
begin
  FileName := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), '..\..\images\Beatles_Trenter_1963.jpg');
  if TFile.Exists(FileName) then
    LoadImage(FileName);
end;

procedure TForm8.LoadImage(const FileName: string);
begin
  ImageForm.Image.LoadFromFile(FileName);
  lbText.Clear;
  lbCelebrities.Clear;
  if not ImageForm.Visible then
  begin
    ImageForm.Top := Self.Top;
    ImageForm.Left := Self.Left + Self.ClientWidth + 1;
  end;
  ImageForm.Show;
end;

end.
