using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data.OleDb;
using System.Linq;
using Dapper;
using NLog;
using psDataImporter.Contracts.Access;

namespace psDataImporter.Data
{
    public class AccessRepository
    {
        private static readonly Logger Logger = LogManager.GetCurrentClassLogger();

        public IEnumerable<Weights> GetWeights()
        {
            IEnumerable<Weights> weights = new List<Weights>();
            try
            {
                using (var conn = new OleDbConnection(ConfigurationManager
                    .ConnectionStrings["accessConnectionString"]
                    .ConnectionString))
                {
                    Logger.Info("Getting weight data");
                    conn.Open();
                    weights = conn.Query<Weights>(
                        @"SELECT *, [DATE] + IIF(ISNULL([TIME]),0,[TIME]) as [TimeMeasured] FROM WEIGHTS"); // null times are turned to a value.
                    conn.Close();
                }
            }
            catch (Exception ex)
            {
                Logger.Error(ex, "Access error" + ex.Message);
            }
            return weights;
        }

        public IEnumerable<Ultrasound> GetUltrasounds()
        {
            IEnumerable<Ultrasound> ultrasounds = new List<Ultrasound>();
            try
            {
                using (var conn = new OleDbConnection(ConfigurationManager
                    .ConnectionStrings["accessConnectionString"]
                    .ConnectionString))
                {
                    Logger.Info("Getting ultrasound data");
                    conn.Open();
                    ultrasounds = conn.Query<Ultrasound>(
                        @"SELECT 
                            [DATE] ,
                            [INDIV] ,
                            [PACK] ,
                            [FOETUS NUMBER] ,
                            [FOETUS SIZE] ,
                            [FOETUS 1 - CROSS VIEW LENGTH] as FOETUS_1_CROSS_VIEW_LENGTH ,
                            [FOETUS 1 - CROSS VIEW WIDTH] as FOETUS_1_CROSS_VIEW_WIDTH ,
                            [FOETUS 1 - LONG VIEW LENGTH] as FOETUS_1_LONG_VIEW_LENGTH ,
                            [FOETUS 1 - LONG VIEW WIDTH] as FOETUS_1_LONG_VIEW_WIDTH ,
                            [FOETUS 2 - CROSS VIEW LENGTH] as FOETUS_2_CROSS_VIEW_LENGTH ,
                            [FOETUS 2 - CROSS VIEW WIDTH] as FOETUS_2_CROSS_VIEW_WIDTH ,
                            [FOETUS 2 - LONG VIEW LENGTH] as FOETUS_2_LONG_VIEW_LENGTH ,
                            [FOETUS 2 - LONG VIEW WIDTH] as FOETUS_2_LONG_VIEW_WIDTH ,
                            [FOETUS 3 -  CROSS VIEW LENGTH] as FOETUS_3_CROSS_VIEW_LENGTH ,
                            [FOETUS 3 - CROSS VIEW WIDTH] as FOETUS_3_CROSS_VIEW_WIDTH ,
                            [FOETUS 3 - LONG VIEW LENGTH] as FOETUS_3_LONG_VIEW_LENGTH ,
                            [FOETUS 3 - LONG VIEW WIDTH] as FOETUS_3_LONG_VIEW_WIDTH ,
                            [FOETUS 4 - CROSS VIEW LENGTH] as FOETUS_4_CROSS_VIEW_LENGTH ,
                            [FOETUS 4 - CROSS VIEW WIDTH] as FOETUS_4_CROSS_VIEW_WIDTH ,
                            [FOETUS 4 - LONG VIEW LENGTH] as FOETUS_4_LONG_VIEW_LENGTH ,
                            [FOETUS 4 - LONG VIEW WIDTH] as FOETUS_4_LONG_VIEW_WIDTH ,
                            [FOETUS 5 - CROSS VIEW LENGTH] as FOETUS_5_CROSS_VIEW_LENGTH ,
                            [FOETUS 5 - CROSS VIEW WIDTH] as FOETUS_5_CROSS_VIEW_WIDTH ,
                            [FOETUS 5 - LONG VIEW LENGTH] as FOETUS_5_LONG_VIEW_LENGTH ,
                            [FOETUS 5 - LONG VIEW WIDTH] as FOETUS_5_LONG_VIEW_WIDTH ,
                            [FOETUS 6 - CROSS VIEW LENGTH] as FOETUS_6_CROSS_VIEW_LENGTH ,
                            [FOETUS 6 - CROSS VIEW WIDTH] as FOETUS_6_CROSS_VIEW_WIDTH ,
                            [FOETUS 6 - LONG VIEW LENGTH] as FOETUS_6_LONG_VIEW_LENGTH ,
                            [FOETUS 6 - LONG VIEW WIDTH] as FOETUS_6_LONG_VIEW_WIDTH ,
                            [OBSERVER] ,
                            [COMMENT]
                            FROM
                            [ULTRASOUND DATA]");
                    conn.Close();
                }
            }
            catch (Exception ex)
            {
                Logger.Error(ex, "Access error" + ex.Message);
            }
            return ultrasounds;
        }

        public IEnumerable<RadioCollar> GetRadioCollars()
        {
            using (var conn = new OleDbConnection(ConfigurationManager
                .ConnectionStrings["accessConnectionString"]
                .ConnectionString))
            {
                Logger.Info("Getting Radio Collar data");
                return conn.Query<RadioCollar>(@" 
                                        SELECT
                                        PACK,
                                        INDIVIDUAL,
                                        FREQUENCY,
                                        [TURNED ON] AS TURNED_ON,
                                        FITTED,
                                        REMOVED,
                                        [WEIGHT(G)] AS WEIGHT,
                                        DATE_ENTERED
                                        FROM [RADIOCOLLAR RECORDS]").ToList();
            }
        }

        public List<NewLifeHistory> GetLifeHistorys()
        {
            using (var conn = new OleDbConnection(ConfigurationManager
                .ConnectionStrings["accessConnectionString"]
                .ConnectionString))
            {
                Logger.Info("Getting Life History data");

                return conn.Query<NewLifeHistory>(@" 
                                      SELECT DATE, 
                                            PACK, 
                                            INDIV, 
                                            SEX, 
                                            [AGE CAT] as AgeCat, 
                                            STATUS, 
                                            [START/END] as StartEnd, 
                                            CODE, 
                                            EXACT, 
                                            LSEEN, 
                                            CAUSE, 
                                            LITTER, 
                                            [PREV NAME] as PrevName, 
                                            COMMENT, 
                                            EDITED, 
                                            Latitude,
                                            Longitude,
                                            date_entered
                                            FROM [NEW LIFE HISTORY];
                                            ").ToList();
            }

        }
    }
}