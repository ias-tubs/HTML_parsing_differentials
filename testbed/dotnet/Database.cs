using System.Runtime.CompilerServices;
using Npgsql;

namespace mxssy;

public class Database : IDisposable
{
    private string connstr = "Host=database;Port=5432;Username=mxssy;Password=allyourmutationsarebelongtous;Database=mxssy";
    private NpgsqlConnection _connection;
    private int _timeout = 900; // 15 min
    public Database()
    {
        this._connection = new NpgsqlConnection(this.connstr);
        this._connection.Open();
    }

    public List<Fragment> GetNextBatch(int sanitizerId)
    {
        List<Fragment> fragments = new List<Fragment>();
        using var cmd = new NpgsqlCommand("select g.id, ts.id as ts_id, g.payload from to_sanitize ts join generations g on g.id = ts.gen_id where ts.sanitizer_id = $1 and ts.sanitized_id is null LIMIT 25000", this._connection);
        cmd.CommandTimeout = this._timeout;
        cmd.Parameters.AddWithValue(sanitizerId);
        using (var reader = cmd.ExecuteReader())
        {
            while (reader.Read())
            {
                var id = reader.GetInt32(0);
                var tsId = reader.GetInt32(1);
                var payload = reader.GetString(2);
                fragments.Add(new Fragment(id, tsId, payload));
            }

        }

        return fragments;
    }

    public void AddSanitizerResult(int generationId, int sanitizerId, SanitizerResult output, bool errored, string errorMessage, int tsId)
    {
        using var cmd = new NpgsqlCommand("call insert_sanitized($1, $2, $3, $4, $5, $6, $7, $8)",
            this._connection);
        cmd.CommandTimeout = this._timeout;
        var done = !output.Clean.Contains("mxss");
        cmd.Parameters.AddWithValue(generationId);
        cmd.Parameters.AddWithValue(sanitizerId);
        cmd.Parameters.AddWithValue(output.Serialized);
        cmd.Parameters.AddWithValue(output.Clean);
        cmd.Parameters.AddWithValue(errored ? 1 : 0);
        cmd.Parameters.AddWithValue(errorMessage);
        cmd.Parameters.AddWithValue(done ? 1 : 0);
        cmd.Parameters.AddWithValue(tsId);
        cmd.ExecuteNonQuery();
    }
    public int GetSanitizerId(string name)
    {
/*        using (var cmd = new NpgsqlCommand("insert into sanitizers (name) values ($1) ON CONFLICT DO NOTHING;",
                   this._connection))
        {
            cmd.CommandTimeout = this._timeout;
            cmd.Parameters.AddWithValue(name);
            cmd.ExecuteNonQuery();
        }*/

        using (var cmd = new NpgsqlCommand("SELECT id from sanitizers where name = $1", this._connection))
        {
            cmd.CommandTimeout = this._timeout;
            cmd.Parameters.AddWithValue(name);
            using (var reader = cmd.ExecuteReader())
            {
                reader.Read();
                return reader.GetInt32(0);
            }
        }

    }

    public void Dispose()
    {
        _connection.Dispose();
    }
}
