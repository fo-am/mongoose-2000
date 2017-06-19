using System;

namespace psDataImporter.Contracts.Access
{
    public class Ultrasound
    {
        public DateTime DATE { get; set; }
        public string INDIV { get; set; }
        public string PACK { get; set; }
        public string FOETUS_NUMBER { get; set; }
        public string FOETUS_SIZE { get; set; }
        public float? FOETUS_1_CROSS_VIEW_LENGTH { get; set; }
        public float? FOETUS_1_CROSS_VIEW_WIDTH { get; set; }
        public float? FOETUS_1_LONG_VIEW_LENGTH { get; set; }
        public float? FOETUS_1_LONG_VIEW_WIDTH { get; set; }
        public float? FOETUS_2_CROSS_VIEW_LENGTH { get; set; }
        public float? FOETUS_2_CROSS_VIEW_WIDTH { get; set; }
        public float? FOETUS_2_LONG_VIEW_LENGTH { get; set; }
        public float? FOETUS_2_LONG_VIEW_WIDTH { get; set; }
        public float? FOETUS_3_CROSS_VIEW_LENGTH { get; set; }
        public float? FOETUS_3_CROSS_VIEW_WIDTH { get; set; }
        public float? FOETUS_3_LONG_VIEW_LENGTH { get; set; }
        public float? FOETUS_3_LONG_VIEW_WIDTH { get; set; }
        public float? FOETUS_4_CROSS_VIEW_LENGTH { get; set; }
        public float? FOETUS_4_CROSS_VIEW_WIDTH { get; set; }
        public float? FOETUS_4_LONG_VIEW_LENGTH { get; set; }
        public float? FOETUS_4_LONG_VIEW_WIDTH { get; set; }
        public float? FOETUS_5_CROSS_VIEW_LENGTH { get; set; }
        public float? FOETUS_5_CROSS_VIEW_WIDTH { get; set; }
        public float? FOETUS_5_LONG_VIEW_LENGTH { get; set; }
        public float? FOETUS_5_LONG_VIEW_WIDTH { get; set; }
        public float? FOETUS_6_CROSS_VIEW_LENGTH { get; set; }
        public float? FOETUS_6_CROSS_VIEW_WIDTH { get; set; }
        public float? FOETUS_6_LONG_VIEW_LENGTH { get; set; }
        public float? FOETUS_6_LONG_VIEW_WIDTH { get; set; }
        public string OBSERVER { get; set; }
        public string COMMENT { get; set; }
    }
}