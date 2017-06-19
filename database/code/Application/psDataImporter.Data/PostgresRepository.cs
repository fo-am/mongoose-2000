using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Linq;
using Dapper;
using NLog;
using Npgsql;
using psDataImporter.Contracts.Access;
using psDataImporter.Contracts.dtos;
using psDataImporter.Contracts.Postgres;

namespace psDataImporter.Data
{
    public class PostgresRepository
    {
        private static readonly Logger Logger = LogManager.GetCurrentClassLogger();

        public List<Pack> GetAllPacks()
        {
            var pgPacks = new List<Pack>();
            using (IDbConnection conn = new NpgsqlConnection(ConfigurationManager
                .ConnectionStrings["postgresConnectionString"]
                .ConnectionString))
            {
                pgPacks = conn
                    .Query<Pack>("Select pack_id as PackId, name, pack_created_date as CreatedDate from mongoose.pack")
                    .ToList();
            }
            return pgPacks;
        }

        public List<Individual> GetAllIndividuals()
        {
            var pgIndividuals = new List<Individual>();
            using (IDbConnection conn = new NpgsqlConnection(ConfigurationManager
                .ConnectionStrings["postgresConnectionString"]
                .ConnectionString))
            {
                pgIndividuals = conn
                    .Query<Individual>(
                        "Select individual_id as IndividualId, litter_id as LitterId, name, sex from mongoose.individual")
                    .ToList();
            }
            return pgIndividuals;
        }

        public void AddWeights(IEnumerable<Weights> weights, List<Individual> pgIndividuals)
        {
            foreach (var weight in weights)
            {
                using (var conn = new NpgsqlConnection(ConfigurationManager
                    .ConnectionStrings["postgresConnectionString"]
                    .ConnectionString))

                {
                    // create geography if lat and long are present.
                    var locationString = "NULL";
                    if (!string.IsNullOrEmpty(weight.Latitude) && !string.IsNullOrEmpty(weight.Longitude))
                    {
                        locationString =
                            $"ST_GeographyFromText('SRID=4326;POINT({weight.Latitude} {weight.Longitude})')";
                    }
                    var sql =
                        "Insert into mongoose.weight (individual_id, weight, time, accuracy, session, collar_weight, comment, location)" +
                        $" values (@IndividualId, @Weight, @Time, @Accuracy, @Session, @CollarWeight, @Comment, {locationString})";

                    using (var cmd = new NpgsqlCommand())
                    {
                        cmd.Connection = conn;
                        conn.Open();
                        cmd.AllResultTypesAreUnknown = true;
                        var individualId = pgIndividuals.Single(i => i.Name == weight.Indiv).IndividualId;

                        cmd.CommandText = sql;

                        cmd.Parameters.AddWithValue("IndividualId", individualId);
                        cmd.Parameters.AddWithValue("Weight", weight.Weight);
                        cmd.Parameters.AddWithValue("Time", weight.TimeMeasured);
                        cmd.Parameters.AddWithValue("Accuracy", (object)weight.Accuracy ?? DBNull.Value);
                        cmd.Parameters.AddWithValue("Session", (object)weight.Session ?? DBNull.Value);
                        cmd.Parameters.AddWithValue("CollarWeight", (object)weight.Collar ?? DBNull.Value);
                        cmd.Parameters.AddWithValue("Comment", (object)weight.Comment ?? DBNull.Value);

                        cmd.ExecuteNonQuery();
                    }
                    Logger.Info($"Insert weight for: {weight.Indiv} weight {weight.Weight}");
                }
            }
        }

        public void InsertPackHistory(int packId, int individualId, PackHistoryDto membership)
        {
            using (IDbConnection conn = new NpgsqlConnection(ConfigurationManager
                .ConnectionStrings["postgresConnectionString"]
                .ConnectionString))

            {
                conn.Execute(
                    "Insert into mongoose.pack_history (pack_id, individual_id, date_joined) values (@PackId, @IndividualId, @DateJoined)",
                    new { PackId = packId, IndividualId = individualId, membership.DateJoined });

                Logger.Info($"Insert pack history: {membership.DateJoined}");
            }
        }

        public void UpdatePackHistory(PackHistoryDto membership, PackHistory packHistory)
        {
            using (IDbConnection conn = new NpgsqlConnection(ConfigurationManager
                .ConnectionStrings["postgresConnectionString"]
                .ConnectionString))

            {
                //if we do then check the date, if our date is older then update with the new older date
                conn.Execute(
                    "update mongoose.pack_history set date_joined = @date where pack_history_id = @packHistoryId",
                    new { date = membership.DateJoined, packHistoryId = packHistory.PackHistoryId });

                Logger.Info($"update pack history: {packHistory.PackHistoryId}");
            }
        }


        public void AddIndividualEventCodes(IEnumerable<string> codes)
        {
            foreach (var code in codes)
            {
                if (!string.IsNullOrEmpty(code))
                {
                    Logger.Info($"Adding individual code: {code}");

                    using (IDbConnection conn = new NpgsqlConnection(ConfigurationManager
                        .ConnectionStrings["postgresConnectionString"]
                        .ConnectionString))
                    {
                        conn.Execute(
                            "Insert into mongoose.individual_event_code (code) values (@codeValue) ON CONFLICT DO NOTHING",
                            new { codeValue = code });
                    }
                }
            }
        }

        public PackHistory GetPackHistory(int packId, int individualId)
        {
            using (IDbConnection conn = new NpgsqlConnection(ConfigurationManager
                .ConnectionStrings["postgresConnectionString"]
                .ConnectionString))

            {
                return conn.Query<PackHistory>(
                    "SELECT pack_history_id as PackHistoryId, pack_id as PackId, individual_id as IndividualId, date_joined as DateJoined from mongoose.pack_history where pack_id = @pack and individual_id = @individual",
                    new { pack = packId, individual = individualId }).FirstOrDefault();
            }
        }


        public void AddIndividuals(IEnumerable<Individual> individuals)
        {
            using (IDbConnection conn = new NpgsqlConnection(ConfigurationManager
                .ConnectionStrings["postgresConnectionString"]
                .ConnectionString))
            {
                foreach (var newIndividual in individuals)
                {
                    if (string.IsNullOrEmpty(newIndividual.Name))
                    {
                        Logger.Warn("Individual with null name.");
                        continue;
                    }
                    // see if we have the individual
                    // if so do we have all the data we have at this point?
                    // if not then update it with what we have...
                    // what if we have the same data but it conflicts (Sex eg)
                    // 
                    var inDatabaseIndividual = conn
                        .Query<Individual>(
                            "Select individual_id as IndividualId, litter_id as LitterId,  name, sex  from mongoose.individual where name = @name",
                            new { newIndividual.Name }).SingleOrDefault();

                    if (inDatabaseIndividual != null)
                    {
                        //If database has no sex, but our new individual does
                        if (string.IsNullOrEmpty(inDatabaseIndividual.Sex) && !string.IsNullOrEmpty(newIndividual.Sex))
                        {
                            Logger.Info(
                                $"Added sex: '{newIndividual.Sex}' to individiual with Id : '{newIndividual.IndividualId}'");

                            conn.Execute("Update mongoose.Individual set sex = @sex where individual_id = @id",
                                new { sex = newIndividual.Sex, id = newIndividual.IndividualId });
                        }
                        // if litter id is set then do the litter thing... worry about that when I have some data!
                    }

                    conn.ExecuteScalar<int>(
                        "Insert into mongoose.individual (name, sex) values (@name, @sex) ON CONFLICT DO NOTHING",
                        new { newIndividual.Name, newIndividual.Sex });
                    Logger.Info($"created indiv: {newIndividual.Name}");
                }
            }
        }

        public void AddPacks(IEnumerable<string> packNames)
        {
            foreach (var packName in packNames)
            {
                if (string.IsNullOrEmpty(packName))
                {
                    Logger.Warn("Tried to create a null pack.");
                    continue;
                }
                using (IDbConnection conn = new NpgsqlConnection(ConfigurationManager
                    .ConnectionStrings["postgresConnectionString"]
                    .ConnectionString))
                {
                    conn.ExecuteScalar<int>("Insert into mongoose.pack (name) values (@name) ON CONFLICT DO NOTHING ",
                        new { name = packName });
                }
                Logger.Info($"created pack: {packName}");
            }
        }

        public void AddFoetus(int individualId, int foetusNumber, DateTime ultrasoundDate, string foetusSize,
            float? crossViewWidth, float? crossViewLength, float? longViewLength, float? longViewWidth,
            string comment, string observer)
        {
            using (IDbConnection conn = new NpgsqlConnection(ConfigurationManager
                .ConnectionStrings["postgresConnectionString"]
                .ConnectionString))
            {
                conn.Execute(
                    "Insert into mongoose.ultrasound (individual_id, observation_date, foetus_number, foetus_size, cross_view_length, cross_view_width, long_view_length, long_view_width, observer, comment)" +
                    " values(@individualId, @observationDate, @foetusNumber, @foetusSize, @crossViewLength, @crossViewWidth, @longViewLength, @longViewWidth, @Observer, @comment)",
                    new
                    {
                        individualId,
                        observationDate = ultrasoundDate,
                        foetusNumber,
                        foetusSize,
                        crossViewWidth,
                        crossViewLength,
                        longViewLength,
                        longViewWidth,
                        comment,
                        observer
                    });
            }
        }

        public void RemoveUltrasoundData()
        {
            using (IDbConnection conn = new NpgsqlConnection(ConfigurationManager
                .ConnectionStrings["postgresConnectionString"]
                .ConnectionString))
            {
                conn.Execute("truncate mongoose.ultrasound");
                Logger.Info("Truncated ultrasound table");
            }
        }

        public void RemoveRadioCollarData()
        {
            using (IDbConnection conn = new NpgsqlConnection(ConfigurationManager
                .ConnectionStrings["postgresConnectionString"]
                .ConnectionString))
            {
                conn.Execute("truncate mongoose.radiocollar");
                Logger.Info("Truncated radiocollar table");
            }
        }

        public void AddRadioCollar(int individualId, DateTime? fitted, DateTime? turnedOn, DateTime? removed,
            int? frequency, int weight, DateTime? dateEntered, string comment)
        {
            using (IDbConnection conn = new NpgsqlConnection(ConfigurationManager
                .ConnectionStrings["postgresConnectionString"]
                .ConnectionString))
            {
                conn.Execute(
                    "insert into mongoose.radiocollar (individual_id, frequency, weight, fitted, turned_on, removed, comment, date_entered) " +
                    "values (@individualId, @frequency, @weight, @fitted, @turnedOn, @removed, @comment, @dateEntered)",
                    new { individualId, frequency, weight, fitted, turnedOn, removed, comment, dateEntered });
                Logger.Info("Added radio collar data.");
            }
        }


        public void AddLitter(LifeHistoryDto litter)
        {
            using (IDbConnection conn = new NpgsqlConnection(ConfigurationManager
                .ConnectionStrings["postgresConnectionString"]
                .ConnectionString))
            {
                //Insert new litter and get back Id
                var litterId = conn.ExecuteScalar<int>(
                    "insert into mongoose.litter (pack_id, name)"
                    + "  values(@packId, @name)"
                    + " on conflict(name) do update set name = @name  RETURNING litter_id",
                    new { packid = litter.pgPackId, name = litter.Litter });

                //update individual with litter id
                conn.Execute(
                    "update mongoose.individual set litter_id = @litterId where individual_id = @individual_id",
                    new { litterId, individual_id = litter.pgIndividualId });

                Logger.Info($"Added litter {litter.Litter}.");
            }
        }

        public void AddPackEventCodes(IEnumerable<string> codes)
        {
            foreach (var code in codes)
            {
                if (!string.IsNullOrEmpty(code))
                {
                    Logger.Info($"Adding pack code:{code}");

                    using (IDbConnection conn = new NpgsqlConnection(ConfigurationManager
                        .ConnectionStrings["postgresConnectionString"]
                        .ConnectionString))
                    {
                        conn.Execute(
                            "Insert into mongoose.pack_event_code (code) values (@codeValue) ON CONFLICT DO NOTHING",
                            new { codeValue = code });
                    }
                }
            }
        }
        public List<IndividualEventCode> GetIndividualCodes()
        {
            using (IDbConnection conn = new NpgsqlConnection(ConfigurationManager
               .ConnectionStrings["postgresConnectionString"]
               .ConnectionString))
            {
                return conn.Query<IndividualEventCode>(
                        "Select individual_event_code_id as IndividualEventCodeId, code from mongoose.individual_event_code")
                    .ToList();
            }
        }
        public List<PackEventCode> GetPackEventCodes()
        {
            using (IDbConnection conn = new NpgsqlConnection(ConfigurationManager
                .ConnectionStrings["postgresConnectionString"]
                .ConnectionString))
            {
                return conn.Query<PackEventCode>(
                        "Select pack_event_code_id as PackEventCodeId, code, detail from mongoose.pack_event_code")
                    .ToList();
            }
        }
        public void LinkIndividualEvents(int individualId, int individualEventCodeId, string latitude, string longitude, string status, DateTime date, string exact, string comment)
        {
            // create geography if lat and long are present.
            var locationString = "NULL";
            if (!string.IsNullOrEmpty(latitude) && !string.IsNullOrEmpty(longitude))
            {
                locationString =
                    $"ST_GeographyFromText('SRID=4326;POINT({latitude} {longitude})')";
            }

            var sql =
                "Insert into mongoose.individual_event (individual_id, individual_event_code_id, status, date, exact, comment, location )" +
                $" values (@individualId, @individualEventCodeId, @status, @date, @exact, @Comment, {locationString})";

            using (IDbConnection conn = new NpgsqlConnection(ConfigurationManager
                .ConnectionStrings["postgresConnectionString"]
                .ConnectionString))
            {
                Logger.Info($"Linking individualId: {individualId} with codeId: {individualEventCodeId}");
                conn.Execute(sql, new { individualId, individualEventCodeId, status, date, exact, comment });
            }
        }
    

        public void linkPackEvents(int packId, int packEventCodeId, string status, DateTime date,
            string exact, string comment, string latitude, string longitude)
        {
            // create geography if lat and long are present.
            var locationString = "NULL";
            if (!string.IsNullOrEmpty(latitude) && !string.IsNullOrEmpty(longitude))
            {
                locationString =
                    $"ST_GeographyFromText('SRID=4326;POINT({latitude} {longitude})')";
            }

            var sql =
                "Insert into mongoose.pack_event (pack_id, pack_event_code_id, status, date, exact, comment, location )" +
                $" values (@PackId, @packEventCodeId, @status, @date, @exact, @Comment, {locationString})";

            using (IDbConnection conn = new NpgsqlConnection(ConfigurationManager
                .ConnectionStrings["postgresConnectionString"]
                .ConnectionString))
            {
                Logger.Info($"Linking packId:{packId} with codeId:{packEventCodeId}");
                conn.Execute(sql, new {packId, packEventCodeId, status, date, exact, comment});
            }
        }
    }
}