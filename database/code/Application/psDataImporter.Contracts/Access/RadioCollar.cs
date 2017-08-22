using System;

namespace psDataImporter.Contracts.Access
{
    public class RadioCollar
    {
        public string PACK { get; set; }
        public string INDIVIDUAL { get; set; }
        public int? FREQUENCY { get; set; }
        public DateTime? TURNED_ON { get; set; }
        public DateTime? FITTED { get; set; }
        public DateTime? REMOVED { get; set; }
        public int WEIGHT { get; set; }
        public string COMMENT { get; set; }
        public DateTime? DATE_ENTERED { get; set; }
    }
}