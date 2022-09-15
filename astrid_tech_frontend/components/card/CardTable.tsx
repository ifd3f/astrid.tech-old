import { FC } from "react";
import { HCardProperty } from "./values";

type CardTableProps = {
  fields: HCardProperty[];
};

export const CardTable: FC<CardTableProps> = ({ fields }) => {
  return (
    <table>
      <tbody>
        {fields.map((f) => (
          <tr key={f.key}>
            <th
              style={{
                textAlign: "right",
                verticalAlign: "top",
                paddingRight: 10,
                minWidth: 120,
              }}
            >
              {f.title}
            </th>
            <td>{f.children}</td>
          </tr>
        ))}
      </tbody>
    </table>
  );
};

export default CardTable;
