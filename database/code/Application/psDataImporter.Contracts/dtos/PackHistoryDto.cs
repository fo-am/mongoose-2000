using System;

namespace psDataImporter.Contracts.dtos
{
    public class PackHistoryDto
    {
        public PackHistoryDto(string individalName, string packName, DateTime dateJoined)
        {
            IndividualName = individalName;
            PackName = packName;
            DateJoined = dateJoined;
        }

        public string IndividualName { get; set; }
        public string PackName { get; set; }
        public DateTime DateJoined { get; set; }
    }
}