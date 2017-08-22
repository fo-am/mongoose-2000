using System;

namespace psDataImporter.Contracts.Access
{
    public class Weights
    {
        public string Group { get; set; }
        public DateTime Date { get; set; }
        public string Indiv { get; set; }
        public string Sex { get; set; }
        public int Weight { get; set; }
        public DateTime Time { get; set; }
        public int? Accuracy { get; set; }
        public string Session { get; set; }
        public int? Collar { get; set; }
        public string Comment { get; set; }
        public string Latitude { get; set; }
        public string Longitude { get; set; }
        public DateTime TimeMeasured { get; set; }
    }
}