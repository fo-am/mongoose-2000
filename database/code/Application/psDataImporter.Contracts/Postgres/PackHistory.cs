using System;

namespace psDataImporter.Contracts.Postgres
{
    public class PackHistory
    {
        public int PackHistoryId { get; set; }
        public int PackId { get; set; }
        public int IndividualId { get; set; }
        public DateTime DateJoined { get; set; }
    }
}