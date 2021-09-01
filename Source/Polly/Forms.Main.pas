unit Forms.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.IOUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit,
  FMX.Media, AWS.Polly, AWS.RegionEndpoints;

type
  TForm7 = class(TForm)
    MediaPlayer1: TMediaPlayer;
    Edit1: TEdit;
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    procedure PlayAudioStream(Stream: TStream);
  public
  end;

var
  Form7: TForm7;

implementation

{$R *.fmx}

procedure TForm7.Button1Click(Sender: TObject);
var
  Client: IAmazonPolly;
  Request: ISynthesizeSpeechRequest;
  Response: ISynthesizeSpeechResponse;
begin
  // Create client
  Client := TAmazonPollyClient.Create(TRegionEndpoints.USWest2);

  // Build request
  Request := TSynthesizeSpeechRequest.Create;
  Request.Text := Edit1.Text;
  Request.VoiceId := TVoiceId.Joanna;
  Request.OutputFormat := TOutputFormat.Mp3;
  Request.LanguageCode := TLanguageCode.EnUS;

  // Retrieve response
  Response := Client.SynthesizeSpeech(Request);

  // Play audio
  PlayAudioStream(Response.AudioStream);
end;

procedure TForm7.PlayAudioStream(Stream: TStream);
const
  BufferSize = 65536;
var
  Buffer: TArray<Byte>;
  FileName: string;
  FileStream: TFileStream;
  BytesRead: Integer;
begin
  FileName := TPath.GetTempFileName;

  // Copy audio stream to file
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    SetLength(Buffer, BufferSize);
    repeat
      BytesRead := Stream.Read(Buffer[0], BufferSize);
      FileStream.Write(Buffer[0], BytesRead);
    until BytesRead < BufferSize;
  finally
    FileStream.Free;
  end;

  // Play audio
  MediaPlayer1.FileName := FileName;
  MediaPlayer1.Play;

  // Delete file
  TFile.Delete(FileName);
end;

end.
