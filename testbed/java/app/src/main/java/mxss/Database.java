package mxss;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class Database implements AutoCloseable {
    private final Connection connection;

    public Database(String host, String userName, String password) throws Exception {
        Class.forName("org.postgresql.Driver");
        String connectionString = String.format("jdbc:postgresql://%s/mxssy", host);

        this.connection = DriverManager.getConnection(connectionString, userName, password);
    }

    public int getSanitizerId(String name) throws SQLException {
        try (PreparedStatement ps = this.connection.prepareStatement("SELECT id from sanitizers where name = ?")) {
            ps.setString(1, name);
            try (ResultSet rs = ps.executeQuery()) {
                rs.next();
                return rs.getInt(1);
            }
        }
    }

    public void addSanitizerResult(long generationId, int sanitizerId,  String output, String serialized, boolean errored, String errorMessage, boolean done, long tsid) throws SQLException {
        String query = "call insert_sanitized(?, ?, ?, ?, ?, ?, ?, ?)";
        try (PreparedStatement ps = this.connection.prepareStatement(query)) {
            ps.setLong(1, generationId);
            ps.setInt(2, sanitizerId);
            ps.setString(3, serialized);
            ps.setString(4, output);
            ps.setInt(5, errored ? 1 : 0);
            ps.setString(6, errorMessage);
            ps.setInt(7, done ? 0 : 1);
            ps.setLong(8, tsid);
            ps.executeUpdate();
        }
    }

    public List<CodeFragment> getNextBatch(int sanitizerId) throws SQLException {
        List<CodeFragment> fragments = new ArrayList<>(100);
        String query = "select ts.id as tsid, g.id, g.payload from to_sanitize ts join generations g on g.id = ts.gen_id where ts.sanitizer_id = ? and ts.sanitized_id is null LIMIT 25000";
        try (PreparedStatement ps = this.connection.prepareStatement(query)) {
            ps.setInt(1, sanitizerId);
            try(ResultSet rs = ps.executeQuery()) {
                while (rs.next()) {
                    long id = rs.getLong(2);
                    long tsid = rs.getLong(1);
                    String payload = rs.getString(3);
                    fragments.add(new CodeFragment(id, tsid, payload));
                }
            }
        }
        return fragments;
    }

    @Override
    public void close() throws Exception {
        if (this.connection != null) {
            this.connection.close();
        }
    }
}
