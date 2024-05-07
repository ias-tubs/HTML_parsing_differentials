namespace mxssy;

public class Fragment
{
    public Fragment(int id, int tsId, string payload)
    {
        this.Id = id;
        this.TsId = tsId;
        this.Payload = payload;
    }
    public int Id { get;  }
    public int TsId { get;  }
    public string Payload { get;  }
}