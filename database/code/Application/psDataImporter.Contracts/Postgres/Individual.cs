namespace psDataImporter.Contracts.Postgres
{
    public class Individual
    {
        public Individual()
        {
        }

        public Individual(int pgIndivididualId, string name, string sex, int? litterId = null)
        {
            IndividualId = pgIndivididualId;
            Name = name;
            LitterId = litterId;
            Sex = sex;
        }

        public int IndividualId { get; set; }
        public string Name { get; set; }
        public string Sex { get; set; }
        public int? LitterId { get; set; }
    }
}